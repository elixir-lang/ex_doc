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
  const results = getSuggestions(term)
  const template = autocompleteResultsTemplate({
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
 * @returns {(Object|null)} jQuery element or null if no autocomplete result is currently selected.
 */
function selectedElement () {
  const currentlySelectedElement = $('.autocomplete-suggestion.selected')
  if (currentlySelectedElement.length === 0) {
    return null
  }

  if (currentlySelectedElement.attr('data-index') === '-1') {
    // -1 marks the deafult 'Search for "phrase"...' element
    return null
  }

  return currentlySelectedElement
}

/**
 * Moves the autocomplete selection up or down.
 * When moving past the last element, selects the first one.
 * When moving before the first element, selects the last one.
 *
 * @param {Number} direction - '-1' to move the selection down, '1' to move it up
 */
function moveSelection (direction) {
  const currentlySelectedElement = $('.autocomplete-suggestion.selected')
  let indexToSelect = -1
  if (currentlySelectedElement.length) {
    indexToSelect = parseInt(currentlySelectedElement.attr('data-index')) + direction
  }

  let elementToSelect = $(`.autocomplete-suggestion[data-index="${indexToSelect}"]`)

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
