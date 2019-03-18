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

function flattenResults (resultsItem) {
  console.log("Flattening ", resultsItem)
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

  console.log("f/c/t", functions, callbacks, types)

  var foundFunctions = [...functions, ...callbacks, ...types]

  var results =
    foundFunctions.filter(function (result) {
      return result.match !== result.id
    })
      .map(function (result) {
        console.log(result)

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

function addTypeName (modules, typeName) {
  return modules.map((module) => {
    module.typeName = typeName
    return module
  })
}

function updateSuggestions (term) {
  var nodes = sidebarNodes
  var safeVal = new RegExp(helpers.escapeText(term), 'i')

  var modules = findIn(nodes.modules, safeVal)
  var exceptions = findIn(nodes.exceptions, safeVal)
  var tasks = findIn(nodes.tasks, safeVal)

  modules = addTypeName(modules, 'Module')
  exceptions = addTypeName(exceptions, 'Exception')
  tasks = addTypeName(tasks, 'Mix Task')

  var results = [...modules, ...exceptions, ...tasks]

  console.log('before flattern', results)

  results = results.reduce(function (acc, resultsItem) {
    return acc.concat(flattenResults(resultsItem))
  }, [])

  console.log('after flatten', results)

  results.sort(function (item1, item2) {
    var priorities = {
      'Module': 3,
      'Function': 2,
      'Exception': 1,
      'Mix Task': 0
    }

    var weight1 = priorities[item1.typeName] || -1
    var weight2 = priorities[item2.typeName] || -1

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
