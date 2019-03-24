// Dependencies
// ------------

import $ from 'jquery'
import autocompleteResultsTemplate from '../templates/autocomplete-suggestions.handlebars'
import { getSuggestions } from './suggestions'

// Constants
// ---------

const autocompleteElement = $('.autocomplete')

// Updates list of results inside the autocomplete.
function updateSuggestions (term) {
  var results = getSuggestions(term)
  var template = autocompleteResultsTemplate({
    empty: results.length === 0,
    results: results,
    term: term
  })

  autocompleteElement.html(template)
}

function hide () {
  autocompleteElement.hide()
}

function show () {
  autocompleteElement.show()
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
  var currentlySelectedElement = $('.autocomplete-suggestion.selected')
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
  var currentlySelectedElement = $('.autocomplete-suggestion.selected')
  var indexToSelect = -1
  if (currentlySelectedElement.length) {
    indexToSelect = parseInt(currentlySelectedElement.attr('data-index')) + direction
  }

  var elementToSelect = $(`.autocomplete-suggestion[data-index="${indexToSelect}"]`)

  if (!elementToSelect.length) {
    if (indexToSelect < 0) {
      elementToSelect = $('.autocomplete-suggestion:last')
    } else {
      elementToSelect = $('.autocomplete-suggestion:first')
    }
  }

  $('.autocomplete-suggestion').each(function () {
    $(this).toggleClass('selected', $(this).is(elementToSelect))
  })
}

// Public Methods
// --------------

export { update, moveSelection, hide, selectedElement }
