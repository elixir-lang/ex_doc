/* globals sidebarNodes */

// Dependencies
// ------------

import $ from 'jquery'
import * as helpers from './helpers'
import {findIn} from './search'
import autocompleteResultsTemplate from './templates/autocomplete-results.handlebars'

// Constants
// ---------

var AUTOCOMPLETE = $('.autocomplete')
var RESULTS_COUNT = 5
var SORTING_PRIORITY = {
  'Module': 3,
  'Child': 2,
  'Exception': 1,
  'Mix Task': 0
}

function addLabel (items, labelName) {
  return items.map((item) => {
    item.label = labelName
    return item
  })
}

function addCategory (modules, category) {
  return modules.map((module) => {
    module.category = category
    return module
  })
}

/**
 * Checks if given module/function matches the search term.
 *
 * @returns {boolean}
 */
function isMatch (item) {
  return item.match !== item.id
}

/**
 * Quick summary
 *
 * Full description.
 *
 * @param {Object} moduleResults results
 */
function parseModuleResults (moduleResults) {
  var functions = moduleResults.functions || []
  var callbacks = moduleResults.callbacks || []
  var types = moduleResults.types || []

  types = addLabel(types, 'type')
  callbacks = addLabel(callbacks, 'callback')

  const results =
    [...functions, ...callbacks, ...types]
      .filter(isMatch)
      .map((item) => serialize(item, moduleResults.id))

  if (isMatch(moduleResults)) {
    const serializedModuleData = serialize(moduleResults, moduleResults.id, false)
    results.unshift(serializedModuleData)
  }

  return results
}

/**
 *
 *
 * @param {Object} item
 * @param {string} moduleId
 * @param {boolean} isChild
 * @returns {Object} Serialized object that can be used directly in the autocomplete template.
 */
function serialize (item, moduleId, isChild = true) {
  const anchor = isChild ? item.anchor : ''
  const category = isChild ? 'Child' : item.category
  const description = isChild ? moduleId : null
  const label = item.label || null

  return {
    anchor: anchor, // i.e. "floor/1", will be used to construct a link to the item.
    title: item.match, // Main text displayed for each autocomplete result.
    moduleTitle: moduleId, // Used to construct a link to the item.
    description: description, // Displayed under the title.
    label: label, // 'Callback' or 'Type' - if set it will be displayed next to the title.
    category: category
    // 'Module', 'Mix Task', 'Exception' or 'Child'.
    // Used to sort the results according to the 'SORTING_PRIORITY'.
    // 'Child' means an item that belongs to a module (like Function, Callback or Type).
  }
}

/**
 * @param {string} [term=''] Text we want to search for.
 *
 * @returns {Object[]}
 */
function find (term = '') {
  if (term.trim().length === 0) {
    return []
  }

  var nodes = sidebarNodes
  var regExp = new RegExp(helpers.escapeText(term), 'i')

  var modules = findIn(nodes.modules, regExp)
  var exceptions = findIn(nodes.exceptions, regExp)
  var tasks = findIn(nodes.tasks, regExp)

  modules = addCategory(modules, 'Module')
  exceptions = addCategory(exceptions, 'Exception')
  tasks = addCategory(tasks, 'Mix Task')

  var results = [...modules, ...exceptions, ...tasks]

  results = results.reduce(function (acc, moduleResults) {
    return acc.concat(parseModuleResults(moduleResults))
  }, [])

  results.sort(function (item1, item2) {
    var weight1 = SORTING_PRIORITY[item1.typeName] || -1
    var weight2 = SORTING_PRIORITY[item2.typeName] || -1

    return weight2 - weight1
  })

  return results.slice(0, RESULTS_COUNT)
}

function updateSuggestions (term) {
  var results = find(term)
  var template = autocompleteResultsTemplate({
    empty: results.length === 0,
    results: results,
    term: term
  })

  AUTOCOMPLETE.html(template)
}

function hide () {
  AUTOCOMPLETE.hide()
}

function show () {
  AUTOCOMPLETE.show()
}

function update (searchTerm) {
  if (!searchTerm) {
    hide()
  } else {
    show()
    updateSuggestions(searchTerm)
  }
}

/**
 * Autocomplete element that was selected using keyboard arrows.
 *
 * @returns {Object} jQuery element
 */
function selectedElement () {
  var currentlySelectedElement = $('.autocomplete-result.selected')
  if (currentlySelectedElement.length === 0) {
    return null
  }

  return currentlySelectedElement
}

/**
 * Moves the autocomplete selection up or down.
 * When moving past the last element, selects the first one.
 * When moving before the first element, selects the last one
 *
 * @param {Number} direction - '-1' to move the selection down, '1' to move it up
 */
function moveSelection (direction) {
  var currentlySelectedElement = $('.autocomplete-result.selected')
  var indexToSelect = -1
  if (currentlySelectedElement.length) {
    indexToSelect = parseInt(currentlySelectedElement.attr('data-index')) + direction
  }

  var elementToSelect = $(`.autocomplete-result[data-index="${indexToSelect}"]`)

  if (!elementToSelect.length) {
    if (indexToSelect < 0) {
      elementToSelect = $('.autocomplete-result:last')
    } else {
      elementToSelect = $('.autocomplete-result:first')
    }
  }

  $('.autocomplete-result').each(function () {
    $(this).toggleClass('selected', $(this).is(elementToSelect))
  })
}

// Public Methods
// --------------

export { find, update, moveSelection, hide, selectedElement }
