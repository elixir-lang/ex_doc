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
  var functions = resultsItem.functions || []
  var callbacks = resultsItem.callbacks || []

  var foundFunctions = [...functions, ...callbacks]

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
          isFunction: true
        }
      })

  if (resultsItem.match !== resultsItem.id) {
    results.unshift({
      anchor: '',
      title: resultsItem.match,
      moduleTitle: resultsItem.id,
      description: '',
      isModule: true
    })
  }

  return results
}

function updateSuggestions (term) {
  var nodes = sidebarNodes
  var safeVal = new RegExp(helpers.escapeText(term), 'i')

  var modules = findIn(nodes.modules, safeVal)
  var exceptions = findIn(nodes.exceptions, safeVal)
  var tasks = findIn(nodes.tasks, safeVal)

  var results = [...modules, ...exceptions, ...tasks]

  console.log('before flattern', results)

  results = results.reduce(function (acc, resultsItem) {
    return acc.concat(flattenResults(resultsItem))
  }, [])

  console.log('after flatten', results)

  results.sort(function (item1, item2) {
    if (item1.isModule) {
      if (item2.isModule) {
        return 0
      }
      return -1
    }

    return 1
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
