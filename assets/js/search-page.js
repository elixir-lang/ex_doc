/* globals searchData */

import lunr from 'lunr'
import { qs, escapeHtmlEntities, isBlank, getQueryParamByName, getProjectNameAndVersion } from './helpers'
import { setSearchInputValue } from './sidebar/sidebar-search'

const EXCERPT_RADIUS = 80

const SEARCH_CONTAINER_SELECTOR = '#search'

/**
 * Performs a full-text search within the documentation
 * for the query in URL params and renders the result.
 *
 * Activates only on the `/search.html` page.
 */
export function initialize () {
  if (window.location.pathname.endsWith('/search.html')) {
    const query = getQueryParamByName('q')
    search(query)
  }
}

async function search (value) {
  if (isBlank(value)) {
    renderResults({ value })
  } else {
    setSearchInputValue(value)

    const index = await getIndex()

    try {
      const results = searchResultsToDecoratedSearchNodes(index.search(value))
      renderResults({ value, results })
    } catch (error) {
      renderResults({ value, errorMessage: error.message })
    }
  }
}

function renderResults ({ value, results, errorMessage }) {
  const searchContainer = qs(SEARCH_CONTAINER_SELECTOR)
  const resultsHtml = Handlebars.templates['search-results']({ value, results, errorMessage })
  searchContainer.innerHTML = resultsHtml
}

async function getIndex () {
  lunr.QueryLexer.termSeparator = /\s+/
  lunr.Pipeline.registerFunction(elixirTokenFunction, 'elixirTokenSplitter')
  lunr.Pipeline.registerFunction(elixirTrimmerFunction, 'elixirTrimmer')
  lunr.Pipeline.registerFunction(hyphenSearchFunction, 'hyphenSearch')

  const cachedIndex = await loadIndex()
  if (cachedIndex) { return cachedIndex }

  const index = createIndex()
  saveIndex(index)
  return index
}

async function loadIndex () {
  try {
    const serializedIndex = sessionStorage.getItem(indexStorageKey())
    if (serializedIndex) {
      const index = await decompress(serializedIndex)
      return lunr.Index.load(index)
    } else {
      return null
    }
  } catch (error) {
    console.error('Failed to load index: ', error)
    return null
  }
}

async function saveIndex (index) {
  try {
    const serializedIndex = await compress(index)
    sessionStorage.setItem(indexStorageKey(), serializedIndex)
  } catch (error) {
    console.error('Failed to save index: ', error)
  }
}

async function compress (index) {
  const stream = new Blob([JSON.stringify(index)], {
    type: 'application/json'
  }).stream().pipeThrough(new window.CompressionStream('gzip'))

  const blob = await new Response(stream).blob()
  const buffer = await blob.arrayBuffer()
  return b64encode(buffer)
}

async function decompress (index) {
  const stream = new Blob([b64decode(index)], {
    type: 'application/json'
  }).stream().pipeThrough(new window.DecompressionStream('gzip'))

  const blob = await new Response(stream).text()
  return JSON.parse(blob)
}

function b64encode (buffer) {
  let binary = ''
  const bytes = new Uint8Array(buffer)
  const len = bytes.byteLength
  for (let i = 0; i < len; i++) {
    binary += String.fromCharCode(bytes[i])
  }
  return window.btoa(binary)
}

function b64decode (str) {
  const binaryString = window.atob(str)
  const len = binaryString.length
  const bytes = new Uint8Array(new ArrayBuffer(len))
  for (let i = 0; i < len; i++) {
    bytes[i] = binaryString.charCodeAt(i)
  }
  return bytes
}

function indexStorageKey () {
  return `idv2:${getProjectNameAndVersion()}`
}

function createIndex () {
  return lunr(function () {
    this.tokenizer.separator = /\s+/
    this.ref('ref')
    this.field('title', { boost: 3 })
    this.field('doc')
    this.field('type')
    this.metadataWhitelist = ['position']
    this.pipeline.remove(lunr.stopWordFilter)
    this.use(hyphenSearch)
    this.use(elixirTokenSplitter)
    this.pipeline.remove(lunr.trimmer)
    this.use(elixirTrimmer)

    searchData.items.forEach(searchNode => {
      this.add(searchNode)
    })
  })
}

function elixirTokenSplitter (builder) {
  builder.pipeline.before(lunr.stemmer, elixirTokenFunction)
  builder.searchPipeline.before(lunr.stemmer, elixirTokenFunction)
}

function elixirTokenFunction (token) {
  const tokens = token
    .toString()
    .split(/\.|\/|_/)
    .map(part => {
      return token.clone().update(() => part)
    })

  if (tokens.length > 1) {
    return [...tokens, token]
  }

  return tokens
}

function elixirTrimmer (builder) {
  builder.pipeline.after(lunr.stemmer, elixirTrimmerFunction)
  builder.searchPipeline.after(lunr.stemmer, elixirTrimmerFunction)
}

function elixirTrimmerFunction (token) {
  // Preserve @ at the beginning of tokens
  return token.update(function (s) {
    return s.replace(/^@?\W+/, '').replace(/\W+$/, '')
  })
}

function hyphenSearchFunction (token) {
  const tokenStr = token.toString()
  if (tokenStr.indexOf('-') < 0) return token

  const tokens = []

  tokens.push(
    token.clone(function (s) {
      return s.replace('-', '')
    })
  )

  tokens.push(token)
  return tokens
}

function hyphenSearch (builder) {
  builder.pipeline.before(lunr.stemmer, hyphenSearchFunction)
  builder.searchPipeline.before(lunr.stemmer, hyphenSearchFunction)
}

function searchResultsToDecoratedSearchNodes (results) {
  return results
    // If the docs are regenerated without changing its version,
    // a reference may have been doc'ed false in the code but
    // still available in the cached index, so we skip it here.
    .filter(result => getSearchNodeByRef(result.ref))
    .map(result => {
      const searchNode = getSearchNodeByRef(result.ref)
      const metadata = result.matchData.metadata
      return {
        ...searchNode,
        metadata,
        excerpts: getExcerpts(searchNode, metadata)
      }
    })
}

function getSearchNodeByRef (ref) {
  return searchData.items.find(searchNode => searchNode.ref === ref) || null
}

function getExcerpts (searchNode, metadata) {
  const { doc } = searchNode
  const searchTerms = Object.keys(metadata)

  const excerpts =
    searchTerms
      .filter(term => 'doc' in metadata[term])
      .map(term => {
        return metadata[term].doc.position
          .map(([sliceStart, sliceLength]) => excerpt(doc, sliceStart, sliceLength))
      })
      .reduce((xs, ys) => xs.concat(ys), []) // flatten

  if (excerpts.length === 0) {
    const beginning = doc.slice(0, EXCERPT_RADIUS * 2) + (EXCERPT_RADIUS * 2 < doc.length ? '...' : '')
    return [beginning]
  }

  return excerpts.slice(0, 1)
}

function excerpt (doc, sliceStart, sliceLength) {
  const startPos = Math.max(sliceStart - EXCERPT_RADIUS, 0)
  const endPos = Math.min(sliceStart + sliceLength + EXCERPT_RADIUS, doc.length)
  return [
    startPos > 0 ? '...' : '',
    doc.slice(startPos, sliceStart),
    '<em>' + escapeHtmlEntities(doc.slice(sliceStart, sliceStart + sliceLength)) + '</em>',
    doc.slice(sliceStart + sliceLength, endPos),
    endPos < doc.length ? '...' : ''
  ].join('')
}
