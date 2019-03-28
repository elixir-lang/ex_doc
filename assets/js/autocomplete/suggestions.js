/* globals sidebarNodes */

// Dependencies
// ------------

import * as helpers from '../helpers'

// Constants
// ---------

const resultsCount = 5
const sortingPriority = {
  'Module': 3,
  'Child': 2,
  'Exception': 1,
  'Mix Task': 0
}
const labels = {
  'callbacks': 'callback',
  'types': 'type'
}

/**
 * Transform an object containing data about a search result and transforms it into a simple
 * data structure that can be used directly in the autocomplete template.
 *
 * @param {Object} item
 * @param {string} moduleId
 * @param {boolean} isChild
 * @returns {Object} Serialized object that can be used directly in the autocomplete template.
 */
function serialize (item, moduleId = null) {
  const isChild = item.category === 'Child'
  const anchor = isChild ? item.anchor : ''
  const category = isChild ? 'Child' : item.category
  const description = isChild ? moduleId : null
  const label = item.label || null
  const link = anchor ? `${moduleId}.html#${anchor}` : `${moduleId}.html`

  return {
    link: link, // Link to the result. Used in the 'href' tag.
    title: item.match, // Main text displayed for each autocomplete result.
    description: description, // Displayed under the title.
    label: label, // 'Callback' or 'Type' - if set it will be displayed next to the title.
    matchQuality: item.matchQuality, // 0..1 - How well result matches the search term. Higher is better.
    category: category
    // 'Module', 'Mix Task', 'Exception' or 'Child'.
    // Used to sort the results according to the 'sortingPriority'.
    // 'Child' means an item that belongs to a module (like Function, Callback or Type).
  }
}

/**
 * @param {string} [term=''] Text we want to search for.
 *
 * @returns {Object[]} sorted results of the search.
 */
function getSuggestions (term = '') {
  if (term.trim().length === 0) {
    return []
  }

  const nodes = sidebarNodes

  let modules = findIn(nodes.modules, term, 'Module')
  let exceptions = findIn(nodes.exceptions, term, 'Exception')
  let tasks = findIn(nodes.tasks, term, 'Mix Task')

  let results = [...modules, ...exceptions, ...tasks]

  results = sort(results)

  return results.slice(0, resultsCount)
}

function sort (items) {
  return items.sort(function (item1, item2) {
    const weight1 = sortingPriority[item1.category] || -1
    const weight2 = sortingPriority[item2.category] || -1

    return weight2 - weight1
  }).sort(function (item1, item2) {
    return item2.matchQuality - item1.matchQuality
  })
}

// WIP: TODO: Add description
function findIn (elements, term, categoryName) {
  const regExp = new RegExp(helpers.escapeText(term), 'i')

  return elements.reduce(function (results, element) {
    const title = element.title
    const titleMatch = title && title.match(regExp)

    if (titleMatch) {
      const parentResult = serialize({
        id: element.id,
        match: highlight(titleMatch),
        category: categoryName,
        matchQuality: matchQuality(titleMatch)
      }, element.id)

      results.push(parentResult)
    }

    if (element.nodeGroups) {
      for (let {key, nodes} of element.nodeGroups) {
        let matches = findNested(nodes, title, regExp, term, key)

        if (Object.keys(matches).length > 0) {
          let foundChildren = Object.values(matches)

          foundChildren = foundChildren.map((child) => {
            child.category = 'Child'
            child.label = labels[key]

            return serialize(child, element.id)
          })

          results = results.concat(foundChildren)
        }
      }
    }

    return results
  }, []).filter((result) => !!result)
}

function highlight (match) {
  return match.input.replace(match, `<em>${match[0]}</em>`)
}

function findNested (elements, parentId, matcher, term, key) {
  return (elements || []).reduce((acc, element) => {
    if (acc[key + element.id]) { return acc }

    // Match things like module.func
    const fullTitle = `${parentId}.${element.id}`
    var fullTitleMatch = !(parentId + '.').match(matcher) && fullTitle.match(matcher)
    var match = element.id && element.id.match(matcher)
    var result = JSON.parse(JSON.stringify(element))

    if (match) {
      result.match = highlight(match)
      result.matchQuality = matchQuality(match)
    } else if (fullTitleMatch) {
      const lastSegment = term.split('.').pop()
      const lastSegmentMatcher = new RegExp(helpers.escapeText(lastSegment), 'i')
      const lastSegmentMatch = element.id.match(lastSegmentMatcher)
      result.matchQuality = matchQuality(lastSegmentMatch)
      result.match = highlight(lastSegmentMatch)
    } else {
      return acc
    }

    acc[key + result.id] = result

    return acc
  }, {})
}

function matchQuality (match) {
  if (!match) {
    return 0
  }

  const textLength = match.input.length

  if (!textLength) {
    return 0
  }

  return match.length / textLength
}

// Public Methods
// --------------

export { getSuggestions }
