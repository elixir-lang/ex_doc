/* globals searchNodes */

import lunr from 'lunr'
import resultsTemplate from './handlebars/templates/search-results.handlebars'
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

function search (value) {
  if (isBlank(value)) {
    renderResults({ value })
  } else {
    setSearchInputValue(value)

    const index = getIndex()

    try {
      // Ignore colons representing field-specific query (https://lunrjs.com/guides/searching.html#fields)
      const queryString = value.replace(':', '')
      const results = searchResultsToDecoratedSearchNodes(index.search(queryString))
      renderResults({ value, results })
    } catch (error) {
      renderResults({ value, errorMessage: error.message })
    }
  }
}

function renderResults ({ value, results, errorMessage }) {
  const searchContainer = qs(SEARCH_CONTAINER_SELECTOR)
  const resultsHtml = resultsTemplate({ value, results, errorMessage })
  searchContainer.innerHTML = resultsHtml
}

function getIndex () {
  const cachedIndex = loadIndex()
  if (cachedIndex) { return cachedIndex }

  const index = createIndex()
  saveIndex(index)
  return index
}

function loadIndex () {
  try {
    const serializedIndex = sessionStorage.getItem(indexStorageKey())
    if (serializedIndex) {
      return lunr.Index.load(JSON.parse(serializedIndex))
    } else {
      return null
    }
  } catch (error) {
    console.error('Failed to load index: ', error)
    return null
  }
}

function saveIndex (index) {
  try {
    const serializedIndex = JSON.stringify(index)
    sessionStorage.setItem(indexStorageKey(), serializedIndex)
  } catch (error) {
    console.error('Failed to save index: ', error)
  }
}

function indexStorageKey () {
  return `index:${getProjectNameAndVersion()}`
}

function createIndex () {
  return lunr(function () {
    this.ref('ref')
    this.field('title', { boost: 3, extractor: titleExtractor })
    this.field('doc')
    this.metadataWhitelist = ['position']
    this.pipeline.remove(lunr.stopWordFilter)
    this.use(elixirTokenSplitter)

    searchNodes.forEach(searchNode => {
      this.add(searchNode)
    })
  })
}

function titleExtractor (document) {
  const { title, type } = document

  if (type === 'function' || type === 'callback' || type === 'type') {
    const modFun = title.replace(/\/\d+$/, '')
    const modOrFun = modFun.replace(/\./g, ' ')
    const parts = title.split('.')
    const funArity = parts[parts.length - 1]

    return `${title} ${modFun} ${modOrFun} ${funArity}`
  }

  return title
}

function elixirTokenSplitter (builder) {
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

  lunr.Pipeline.registerFunction(elixirTokenFunction, 'elixirTokenSplitter')
  builder.pipeline.before(lunr.stemmer, elixirTokenFunction)
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
  return searchNodes.find(searchNode => searchNode.ref === ref) || null
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
