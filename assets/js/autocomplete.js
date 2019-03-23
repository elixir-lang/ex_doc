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
var SORTING_PRIORITIES = {
  'Module': 3,
  'Function': 2,
  'Exception': 1,
  'Mix Task': 0
}

function addLabel (items, labelName) {
  return items.map((item) => {
    item.label = labelName
    return item
  })
}

function addTypeName (modules, typeName) {
  return modules.map((module) => {
    module.typeName = typeName
    return module
  })
}

/**
 * Returns `true` if the given module/function name matches the search query.
 */
function isMatch (item) {
  return item.match !== item.id
}

/**
 * Extracts data about functions, callbacks and types.
 *
 * This function replaces an empty unordered list with an
 * unordered list full of links to the different tasks, exceptions
 * and modules mentioned in the documentation.
 *
 * @param {Object} module - module, exception or mix task
 */

function parseModuleResults (moduleResults) {
  console.log(moduleResults)
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
 * @param {Object} item
 * @param {String} moduleId
 * @param {Boolean} isFunction
 * @returns {Object} Serialized object that can be used directly in the autocomplete template
 */

function serialize (item, moduleId, isFunction = true) {
  const anchor = isFunction ? item.anchor : ''
  const typeName = isFunction ? 'Function' : item.typeName
  const description = isFunction ? moduleId : null
  const label = item.label || null

  return {
    anchor: anchor, // ie "floor/1", will be used to construct a link to the item.
    title: item.match, // Main text displayed for each autocomplete result.
    moduleTitle: moduleId, // Used to construct a link to the item.
    description: description, // Displayed under the title.
    label: label, // ie "Callback", special label dispyed next to the title.
    typeName: typeName
  }
}

function find (term = '') {
  if (term.trim().length === 0) {
    return []
  }

  var nodes = sidebarNodes
  var regExp = new RegExp(helpers.escapeText(term), 'i')

  var modules = findIn(nodes.modules, regExp)
  var exceptions = findIn(nodes.exceptions, regExp)
  var tasks = findIn(nodes.tasks, regExp)

  modules = addTypeName(modules, 'Module')
  exceptions = addTypeName(exceptions, 'Exception')
  tasks = addTypeName(tasks, 'Mix Task')

  var results = [...modules, ...exceptions, ...tasks]

  results = results.reduce(function (acc, moduleResults) {
    return acc.concat(parseModuleResults(moduleResults))
  }, [])

  results.sort(function (item1, item2) {
    var weight1 = SORTING_PRIORITIES[item1.typeName] || -1
    var weight2 = SORTING_PRIORITIES[item2.typeName] || -1

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

function hideAutocomplete () {
  AUTOCOMPLETE.hide()
}

function showAutocomplete () {
  AUTOCOMPLETE.show()
}

function updateAutocomplete (searchTerm) {
  if (!searchTerm) {
    hideAutocomplete()
  } else {
    showAutocomplete()
    updateSuggestions(searchTerm)
  }
}

function selectedAutocompleteElement () {
  var currentlySelectedElement = $('.autocomplete-result.selected')
  if (currentlySelectedElement.length === 0) {
    return null
  }

  return currentlySelectedElement
}

function moveAutocompleteSelection (direction) {
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

export { find, updateAutocomplete, moveAutocompleteSelection, hideAutocomplete, selectedAutocompleteElement }
