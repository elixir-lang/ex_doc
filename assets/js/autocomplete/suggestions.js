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
 * Takes an object containing data about a search result and transforms it into a simple
 * data structure that can be used directly in the autocomplete template.
 *
 * @param {Object} item Result to be serialized
 * @param {(string|null)} [moduleId=null] Id of the parent module. If null it means we are serializing the parent module info.
 *
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
    // 'Module', 'Mix Task' or 'Child'.
    // Used to sort the results according to the 'sortingPriority'.
    // 'Child' means an item that belongs to a module (like Function, Callback or Type).
  }
}

/**
 * @param {string} [term=''] Text we want to search for.
 *
 * @returns {Object[]} List of suggestions sorted and limited to 5.
 */
function getSuggestions (term = '') {
  if (term.trim().length === 0) {
    return []
  }

  const nodes = sidebarNodes

  let modules = findIn(nodes.modules, term, 'Module')
  let tasks = findIn(nodes.tasks, term, 'Mix Task')

  let results = [...modules, ...tasks]

  results = sort(results)

  return results.slice(0, resultsCount)
}

/**
 * Sorts suggestions, putting best results first.
 *
 * @param {Object[]} items Unsorted list of results.
 *
 * @returns {Object[]} Results sorted according to match quality and category.
 */
function sort (items) {
  return items.sort(function (item1, item2) {
    const weight1 = getSortingPriority(item1)
    const weight2 = getSortingPriority(item2)

    return weight2 - weight1
  }).sort(function (item1, item2) {
    const matchQuality1 = item1.matchQuality || 0
    const matchQuality2 = item2.matchQuality || 0

    return matchQuality2 - matchQuality1
  })
}

/**
 * Finds matching results in a list of elements.
 *
 * @param {Object[]} elements Array containing information about modules/exceptions/tasks.
 * @param {string} term Text we are searching form
 * @param {string} category "Module"/"Exception"/"Mix Task" - category that elements belong to.
 *
 * @returns {Object[]} List of elements matching the provided term.
 */
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
        matchQuality: matchQuality(titleMatch),
        group: element.group
      }, element.id)

      results.push(parentResult)
    }

    if (element.nodeGroups) {
      for (let {key, nodes} of element.nodeGroups) {
        let matches = findMatchingChildren(nodes, title, term, key)

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

/**
 * Highlight matching part of the string.
 *
 * @param {Array} match Information about the matched text (returned by String.match()).
 *
 * @returns {string} Text with matching part highlighted with html <em> tag.
 */
function highlight (match) {
  return match.input.replace(match, `<em>${match[0]}</em>`)
}

/**
 * Find all matches in the list of elements belonging to a given module.
 *
 * @param {Object[]} elements List of elements
 * @param {string} parentId Id of the Module that elements belong to.
 * @param {string} term Search term
 * @param {string} key Key of the module group we are checking (i.e. "callbacks", "types")
 *
 * @returns {Object[]} List of elements matching the provided term.
 */
function findMatchingChildren (elements, parentId, term, key) {
  const regExp = new RegExp(helpers.escapeText(term), 'i')

  return (elements || []).reduce((acc, element) => {
    if (acc[key + element.id]) { return acc }

    // Match "Module.funcion" format.
    const fullTitle = `${parentId}.${element.id}`
    const fullTitleMatch = !(parentId + '.').match(regExp) && fullTitle.match(regExp)
    const match = element.id && element.id.match(regExp)
    let result = JSON.parse(JSON.stringify(element))

    if (match) {
      result.match = highlight(match)
      result.matchQuality = matchQuality(match)
    } else if (fullTitleMatch) {
      // When match spans both module and function name (i.e. ">Map.fe<tch")
      // let's return just ">fe<tch" as the title. Module will already be displayed under the title.
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

/**
 * Chooses the right sorting priority for the given item.
 *
 * @param {Object} item Search result item.
 *
 * @returns {number} Sorting priority for a given item. Higher priority means higher position in search results.
 */
function getSortingPriority (item) {
  if (isException(item)) {
    return sortingPriority['Exception']
  }

  return sortingPriority[item.category] || -1
}

/**
 * Is the given item an exception?
 *
 * @param {Object} item Search result item.
 *
 * @returns {boolean}
 */
function isException (item) {
  return item.group === 'Exceptions'
}

/**
 * How well search result marches the current query.
 *
 * @param {(Array|null)} match Information about the matched text (returned by String.match()).
 *
 * @returns {number} (0..1) Match quality. Higher is better.
 */
function matchQuality (match) {
  if (!match) { return 0 }

  const textLength = match.input.length

  if (!textLength) { return 0 }

  return match.length / textLength
}

// Public Methods
// --------------

export { getSuggestions }
