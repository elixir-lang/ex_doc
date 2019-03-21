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

/**
 * Transform into a non-nested array.
 *
 * This function replaces an empty unordered list with an
 * unordered list full of links to the different tasks, exceptions
 * and modules mentioned in the documentation.
 *
 * @param {Object} resultItem - module, exception or mix task
 */

function flattenResultItem (resultsItem) {
  var functions = resultsItem.functions || []
  var callbacks = resultsItem.callbacks || []
  var types = resultsItem.types || []

  types = types.map((type) => {
    type.prefix = 'type'
    return type
  })

  callbacks = callbacks.map((callback) => {
    callback.prefix = 'callback'
    return callback
  })

  var foundFunctions = [...functions, ...callbacks, ...types]

  var results =
    foundFunctions.filter(function (result) {
      return result.match !== result.id
    })
      .map(function (result) {
        return {
          anchor: result.anchor,
          title: result.match,
          moduleTitle: resultsItem.id,
          description: '',
          isFunction: true,
          typeName: 'Function',
          prefix: result.prefix
        }
      })

  if (resultsItem.match !== resultsItem.id) {
    results.unshift({
      anchor: '',
      title: resultsItem.match,
      moduleTitle: resultsItem.id,
      description: '',
      isModule: true,
      typeName: resultsItem.typeName
    })
  }

  return results
}

function serializeItem() {
  return {}
}

function addTypeName (modules, typeName) {
  return modules.map((module) => {
    module.typeName = typeName
    return module
  })
}

function updateSuggestions (term) {
  var nodes = sidebarNodes
  var regExp = new RegExp(helpers.escapeText(term), 'i')

  var modules = findIn(nodes.modules, regExp)
  var exceptions = findIn(nodes.exceptions, regExp)
  var tasks = findIn(nodes.tasks, regExp)

  modules = addTypeName(modules, 'Module')
  exceptions = addTypeName(exceptions, 'Exception')
  tasks = addTypeName(tasks, 'Mix Task')

  var results = [...modules, ...exceptions, ...tasks]

  results = results.reduce(function (acc, resultItem) {
    return acc.concat(flattenResultItem(resultItem))
  }, [])

  results.sort(function (item1, item2) {
    var weight1 = SORTING_PRIORITIES[item1.typeName] || -1
    var weight2 = SORTING_PRIORITIES[item2.typeName] || -1

    return weight2 - weight1
  })

  results = results.slice(0, RESULTS_COUNT)

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

export { updateAutocomplete, moveAutocompleteSelection, hideAutocomplete, selectedAutocompleteElement }
