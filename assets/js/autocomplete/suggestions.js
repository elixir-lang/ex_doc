import { getSidebarNodes } from '../globals'
import { escapeRegexModifiers, escapeHtmlEntities, isBlank } from '../helpers'

/**
 * @typedef Suggestion
 * @type {Object}
 * @property {String} link URL of the relevant documentation page.
 * @property {String} title A title of the documentation page.
 * @property {String|null} label A short text describing suggestion type (to be displayed alongside title).
 * @property {String|null} description An additional information (to be displayed below the title).
 * @property {Number} matchQuality How well the suggestion matches the given query string.
 * @property {String} category The group of suggestions that the suggestion belongs to.
 */

const SUGGESTION_CATEGORY = {
  module: 'module',
  moduleChild: 'module-child',
  mixTask: 'mix-task',
  extra: 'extra'
}

/**
 * Returns a list of autocomplete suggestion objects matching the given term.
 *
 * @param {String} query The query string to search for.
 * @param {Number} limit The maximum number of results to return.
 * @returns {Suggestion[]} List of suggestions sorted and limited.
 */
export function getSuggestions (query, limit = 5) {
  if (isBlank(query)) {
    return []
  }

  const nodes = getSidebarNodes()

  const suggestions = [
    ...findSuggestionsInTopLevelNodes(nodes.modules, query, SUGGESTION_CATEGORY.module),
    ...findSuggestionsInChildNodes(nodes.modules, query, SUGGESTION_CATEGORY.moduleChild),
    ...findSuggestionsInTopLevelNodes(nodes.tasks, query, SUGGESTION_CATEGORY.mixTask),
    ...findSuggestionsInTopLevelNodes(nodes.extras, query, SUGGESTION_CATEGORY.extra)
  ]

  return sort(suggestions).slice(0, limit)
}

/**
 * Finds suggestions in top level sidebar nodes.
 */
function findSuggestionsInTopLevelNodes (nodes, query, category) {
  return nodes
    .map(node => nodeSuggestion(node, query, category))
    .filter(suggestion => suggestion !== null)
}

/**
 * Finds suggestions in node groups of the given parent nodes.
 */
function findSuggestionsInChildNodes (nodes, query, category) {
  return nodes
    .filter(node => node.nodeGroups)
    .flatMap(node => {
      return node.nodeGroups.flatMap(({ key, nodes: childNodes }) => {
        const label = nodeGroupKeyToLabel(key)

        return childNodes.map(childNode =>
          childNodeSuggestion(childNode, node.id, query, category, label) ||
          moduleChildNodeSuggestion(childNode, node.id, query, category, label)
        )
      })
    })
    .filter(suggestion => suggestion !== null)
}

/**
 * Builds a suggestion for a top level node.
 * Returns null if the node doesn't match the query.
 */
function nodeSuggestion (node, query, category) {
  if (!matchesAll(node.title, query)) { return null }

  return {
    link: `${node.id}.html`,
    title: highlightMatches(node.title, query),
    label: null,
    description: null,
    matchQuality: matchQuality(node.title, query),
    category: category
  }
}

/**
 * Builds a suggestion for a child node.
 * Returns null if the node doesn't match the query.
 */
function childNodeSuggestion (childNode, parentId, query, category, label) {
  if (!matchesAll(childNode.id, query)) { return null }

  return {
    link: `${parentId}.html#${childNode.anchor}`,
    title: highlightMatches(childNode.id, query),
    label: label,
    description: parentId,
    matchQuality: matchQuality(childNode.id, query),
    category: category
  }
}

/**
 * Builds a suggestion for a child node assuming the parent node is a module.
 * Returns null if the node doesn't match the query.
 */
function moduleChildNodeSuggestion (childNode, parentId, query, category, label) {
  // Match "Module.function" format.
  const modFun = `${parentId}.${childNode.id}`
  if (!matchesAll(modFun, query)) { return null }

  // When match spans both module and function name (i.e. ">Map.fe<tch")
  // let's return just ">fe<tch" as the title.
  // Module will already be displayed as the description under the title.
  const tokenizedQuery = query.replace(/\./g, ' ')
  // Make sure some token actually matches the child id (and not just the module prefix).
  if (!matchesAny(childNode.id, tokenizedQuery)) return null

  return {
    link: `${parentId}.html#${childNode.anchor}`,
    title: highlightMatches(childNode.id, tokenizedQuery),
    label: label,
    description: parentId,
    matchQuality: matchQuality(modFun, query),
    category: category
  }
}

function nodeGroupKeyToLabel (key) {
  switch (key) {
    case 'callbacks': return 'callback'
    case 'types': return 'type'
    default: return null
  }
}

/**
 * Sorts suggestions, putting most accurate results first
 * (according to match quality and suggestion category).
 */
function sort (suggestions) {
  return suggestions.slice().sort((suggestion1, suggestion2) => {
    if (suggestion1.matchQuality !== suggestion2.matchQuality) {
      return suggestion2.matchQuality - suggestion1.matchQuality
    } else {
      return categoryPriority(suggestion1.category) - categoryPriority(suggestion2.category)
    }
  })
}

/**
 * Returns a priority for the given suggestion category. The lower the better.
 */
function categoryPriority (category) {
  switch (category) {
    case SUGGESTION_CATEGORY.module: return 1
    case SUGGESTION_CATEGORY.moduleChild: return 2
    case SUGGESTION_CATEGORY.mixTask: return 3
    default: return 4
  }
}

/**
 * Checks if the given text matches any token from the query.
 */
function matchesAny (text, query) {
  const terms = tokenize(query)
  return terms.some(term => includes(text, term))
}

/**
 * Checks if the given text matches all tokens from the query.
 */
function matchesAll (text, query) {
  const terms = tokenize(query)
  return terms.every(term => includes(text, term))
}

/**
 * Case-insensitive inclusion check.
 */
function includes (text, subtext) {
  return text.toLowerCase().includes(subtext.toLowerCase())
}

/**
 * Match quality metric, the higher the better.
 */
function matchQuality (text, query) {
  const terms = tokenize(query)
  const termsLength = terms.map(term => term.length).reduce((x, y) => x + y, 0)

  const quality = termsLength / text.length
  // Add bonus points if the query matches text at the very start.
  const bonus = startsWith(text, terms[0]) ? 1 : 0

  return quality + bonus
}

/**
 * Case-insensitive `String.startsWith`.
 */
function startsWith (text, subtext) {
  return text.toLowerCase().startsWith(subtext.toLowerCase())
}

/**
 * Returns a list of tokens from the given query string.
 */
function tokenize (query) {
  return query.trim().split(/\s+/)
}

/**
 * Returns an HTML string highlighting the individual tokens from the query string.
 */
function highlightMatches (text, query) {
  // Sort terms length, so that the longest are highlighted first.
  const terms = tokenize(query).sort((term1, term2) => term2.length - term1.length)
  return highlightTerms(text, terms)
}

function highlightTerms (text, terms) {
  if (terms.length === 0) return text

  const [firstTerm, ...otherTerms] = terms
  const match = text.match(new RegExp(`(.*)(${escapeRegexModifiers(firstTerm)})(.*)`, 'i'))

  if (match) {
    const [, before, matching, after] = match
    // Note: this has exponential complexity, but we expect just a few terms, so that's fine.
    return highlightTerms(before, terms) + '<em>' + escapeHtmlEntities(matching) + '</em>' + highlightTerms(after, terms)
  } else {
    return highlightTerms(text, otherTerms)
  }
}
