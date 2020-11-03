// Dependencies
// ------------

import autocompleteResultsTemplate from '../templates/autocomplete-suggestions.handlebars'
import {getSuggestions} from './suggestions'
import {qs, qsAll} from '../helpers'

// Constants
// ---------

const autocompleteElement = qs('.autocomplete')

// Updates list of results inside the autocomplete.
function updateSuggestions (term) {
  const results = getSuggestions(term)
  const template = autocompleteResultsTemplate({
    empty: results.length === 0,
    results: results,
    term: term
  })

  autocompleteElement.innerHTML = template
}

function hide () {
  autocompleteElement.classList.remove('shown')
}

function show () {
  autocompleteElement.classList.add('shown')
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
 * @returns {(Object|null)} element or null if no autocomplete result is currently selected.
 */
function selectedElement () {
  const currentlySelectedElement = qs('.autocomplete-suggestion.selected')
  if (!currentlySelectedElement) {
    return null
  }

  if (currentlySelectedElement.dataset.index === '-1') {
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
  const currentlySelectedElement = qs('.autocomplete-suggestion.selected')
  let indexToSelect = -1
  if (currentlySelectedElement) {
    indexToSelect = parseInt(currentlySelectedElement.dataset.index) + direction
  }

  let elementToSelect = qs(`.autocomplete-suggestion[data-index="${indexToSelect}"]`)

  if (!elementToSelect) {
    if (indexToSelect < 0) {
      elementToSelect = qs('.autocomplete-suggestion:last-child')
    } else {
      elementToSelect = qs('.autocomplete-suggestion:first-child')
    }
  }

  qsAll('.autocomplete-suggestion').forEach(suggestion => {
    suggestion.classList.toggle('selected', suggestion === elementToSelect)
  })
}

// Public Methods
// --------------

export { update, moveSelection, hide, selectedElement }
