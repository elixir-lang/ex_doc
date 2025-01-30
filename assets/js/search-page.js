/* globals searchData */

import lunr from 'lunr'
import { qs, escapeHtmlEntities, isBlank, getQueryParamByName, getProjectNameAndVersion } from './helpers'
import { setSearchInputValue } from './search-bar'
import searchResultsTemplate from './handlebars/templates/search-results.handlebars'
import { getSearchNodes } from './globals'

const EXCERPT_RADIUS = 80
const SEARCH_CONTAINER_SELECTOR = '#search'

lunr.tokenizer.separator = /\s+/
lunr.QueryLexer.termSeparator = /\s+/
lunr.Pipeline.registerFunction(docTokenFunction, 'docTokenSplitter')
lunr.Pipeline.registerFunction(docTrimmerFunction, 'docTrimmer')

/**
 * Performs a full-text search within the documentation
 * for the query in URL params and renders the result.
 *
 * Activates only on the `/search.html` page.
 */

window.addEventListener('exdoc:loaded', initialize)

function initialize () {
  const pathname = window.location.pathname
  if (pathname.endsWith('/search.html') || pathname.endsWith('/search')) {
    const query = getQueryParamByName('q')
    const queryType = getQueryParamByName('type')
    search(query, queryType)
  }
}

async function search (value, queryType) {
  if (isBlank(value)) {
    renderResults({ value })
  } else {
    setSearchInputValue(value)

    try {
      let results = []
      const searchNodes = getSearchNodes()

      if (['related', 'latest'].includes(queryType) && searchNodes.length > 0) {
        results = await remoteSearch(value, queryType, searchNodes)
      } else {
        results = await localSearch(value)
      }

      renderResults({ value, results })
    } catch (error) {
      renderResults({ value, errorMessage: error.message })
    }
  }
}

async function localSearch (value) {
  const index = await getIndex()

  // We cannot match on atoms :foo because that would be considered
  // a filter. So we escape all colons not preceded by a word.
  const fixedValue = value.replaceAll(/(\B|\\):/g, '\\:')
  return searchResultsToDecoratedSearchItems(index.search(fixedValue))
}

async function remoteSearch (value, queryType, searchNodes) {
  let filterNodes = searchNodes

  if (queryType === 'latest') {
    filterNodes = searchNodes.slice(0, 1)
  }

  const filters = filterNodes.map(node => `${node.name}-${node.version}`).join(',')

  const params = new URLSearchParams()
  params.set('q', value)
  params.set('query_by', 'title,doc')
  params.set('filter_by', `package:=[${filters}]`)

  const response = await fetch(`https://search.hexdocs.pm/?${params.toString()}`)
  const payload = await response.json()

  if (Array.isArray(payload.hits)) {
    return payload.hits.map(result => {
      const [packageName, packageVersion] = result.document.package.split('-')

      const doc = result.document.doc
      const excerpts = [doc]
      const metadata = {}
      const ref = `https://hexdocs.pm/${packageName}/${packageVersion}/${result.document.ref}`
      const title = result.document.title
      const type = result.document.type

      return {
        doc,
        excerpts,
        metadata,
        ref,
        title,
        type
      }
    })
  } else {
    return []
  }
}

function renderResults ({ value, results, errorMessage }) {
  const searchContainer = qs(SEARCH_CONTAINER_SELECTOR)
  const resultsHtml = searchResultsTemplate({ value, results, errorMessage })
  searchContainer.innerHTML = resultsHtml
}

async function getIndex () {
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
  return `idv5:${getProjectNameAndVersion()}`
}

function createIndex () {
  return lunr(function () {
    this.ref('ref')
    this.field('title', { boost: 3 })
    this.field('doc')
    this.field('type')
    this.metadataWhitelist = ['position']
    this.pipeline.remove(lunr.stopWordFilter)
    this.pipeline.remove(lunr.trimmer)
    this.use(docTokenSplitter)
    this.use(docTrimmer)

    searchData.items.forEach(searchItem => {
      this.add(searchItem)
    })
  })
}

function docTokenSplitter (builder) {
  builder.pipeline.before(lunr.stemmer, docTokenFunction)
}

function docTokenFunction (token) {
  // If we have something with an arity, we split on : . to make partial
  // matches easier. We split only when tokenizing, not when searching.
  // Below we use ExDoc.Markdown.to_ast/2 as an example.
  const tokens = [token]
  const arityRegex = /\/\d+$/
  const namespaceRegex = /\:|\./
  let toSplitWords = token.toString()

  // remove punctuation at start and end, except for !?
  // at the end which can be part of the identifier itself
  toSplitWords.replace(/^[.,;?!]+|[.,;]+$/g, '')

  // clean up leading and trailing backticks
  if (toSplitWords.startsWith('`') && toSplitWords.endsWith('`')) {
    toSplitWords = toSplitWords.slice(1, -1)
  }

  if (arityRegex.test(toSplitWords)) {
    const withoutArity = token
      .toString()
      .replace(arityRegex, '')

    // This token represents ExDoc.Markdown.to_ast
    tokens.push(token.clone().update(() => withoutArity))

    // And now we get each part as token: ExDoc, Markdown, and to_ast
    const parts = withoutArity.split(namespaceRegex)

    if (parts.length > 1) {
      for (const part of parts) {
        tokens.push(token.clone().update(() => part))
      }

      // Let's also add to_ast/2
      const lastWithArity = token.toString().split(namespaceRegex)
      tokens.push(token.clone().update(() => lastWithArity[lastWithArity.length - 1]))
    }

    toSplitWords = parts[parts.length - 1]
  } else if (toSplitWords.startsWith('@')) {
    // If we have a module attribute, such as @foo_bar,
    // also make it searchable as foo_bar
    toSplitWords = toSplitWords.substring(1)
    tokens.push(token.clone().update(() => toSplitWords))
  } else if (toSplitWords.startsWith(':')) {
    // allow searching for atoms without `:`
    toSplitWords = toSplitWords.substring(1)
    tokens.push(token.clone().update(() => toSplitWords))
  }

  // Now split the function name (or the token, if that's all we had),
  // on _ or - (but we keep the original)
  const words = toSplitWords.split(/\_|\-/)

  if (words.length > 1) {
    for (const word of words) {
      tokens.push(token.clone().update(() => word))
    }
  }

  return tokens
}

function docTrimmer (builder) {
  builder.pipeline.before(lunr.stemmer, docTrimmerFunction)
}

function docTrimmerFunction (token) {
  // Preserve @ and : at the beginning of tokens,
  // and ? and ! at the end of tokens. It needs to
  // be done before stemming, otherwise search and
  // indexes are not equally stemmed.
  return token.update(function (s) {
    return s.replace(/^[^@:\w]+/, '').replace(/[^\?\!\w]+$/, '')
  })
}

function searchResultsToDecoratedSearchItems (results) {
  return results
    // If the docs are regenerated without changing its version,
    // a reference may have been doc'ed false in the code but
    // still available in the cached index, so we skip it here.
    .filter(result => getSearchItemByRef(result.ref))
    .map(result => {
      const searchItem = getSearchItemByRef(result.ref)
      const metadata = result.matchData.metadata
      return {
        ...searchItem,
        metadata,
        excerpts: getExcerpts(searchItem, metadata)
      }
    })
}

function getSearchItemByRef (ref) {
  return searchData.items.find(searchItem => searchItem.ref === ref) || null
}

function getExcerpts (searchItem, metadata) {
  const { doc } = searchItem
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
