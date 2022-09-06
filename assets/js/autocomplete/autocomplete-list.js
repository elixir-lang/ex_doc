import { getSuggestions } from './suggestions'
import { isBlank, qs } from '../helpers'

export const AUTOCOMPLETE_CONTAINER_SELECTOR = '.autocomplete'
export const AUTOCOMPLETE_SUGGESTION_SELECTOR = '.autocomplete-suggestion'

const state = {
  autocompleteSuggestions: [],
  selectedIdx: -1
}

/**
 * Opens the search autocomplete list.
 */
export function showAutocompleteList () {
  qs(AUTOCOMPLETE_CONTAINER_SELECTOR).classList.add('shown')
}

/**
 * Closes the search autocomplete list.
 */
export function hideAutocompleteList () {
  qs(AUTOCOMPLETE_CONTAINER_SELECTOR).classList.remove('shown')
}

/**
 * Checks if the search autocomplete list is open.
 */
export function isAutocompleteListOpen () {
  return qs(AUTOCOMPLETE_CONTAINER_SELECTOR).classList.contains('shown')
}

/**
 * Shows autocomplete suggestions for the given term.
 *
 * For blank terms the list is not shown.
 *
 * @param {String} searchTerm The term to show suggestions for.
 */
export function updateAutocompleteList (searchTerm) {
  state.autocompleteSuggestions = getSuggestions(searchTerm)
  state.selectedIdx = -1

  if (!isBlank(searchTerm)) {
    renderSuggestions({ term: searchTerm, suggestions: state.autocompleteSuggestions })
    // Highlight the first option
    moveAutocompleteSelection(0)
    showAutocompleteList()
  } else {
    hideAutocompleteList()
  }
}

// Updates list of suggestions inside the autocomplete.
function renderSuggestions ({ term, suggestions }) {
  const autocompleteContainerHtml = Handlebars.templates['autocomplete-suggestions']({ suggestions, term })

  const autocompleteContainer = qs(AUTOCOMPLETE_CONTAINER_SELECTOR)
  autocompleteContainer.innerHTML = autocompleteContainerHtml
}

/**
 * Returns an object with the currently selected autocomplete suggestion.
 *
 * @returns {Object|null} Either an autocomplete suggestion or null if none is selected.
 */
export function selectedAutocompleteSuggestion () {
  // Include the default first option with index -1
  if (state.selectedIdx === -1) return null

  return state.autocompleteSuggestions[state.selectedIdx]
}

/**
 * Moves the autocomplete selection up or down.
 *
 * @param {Number} offset How much to move down (or up) the list.
 */
export function moveAutocompleteSelection (offset) {
  state.selectedIdx = newAutocompleteIndex(offset)

  const selectedElement = qs(`${AUTOCOMPLETE_SUGGESTION_SELECTOR}.selected`)
  const elementToSelect = qs(`${AUTOCOMPLETE_SUGGESTION_SELECTOR}[data-index="${state.selectedIdx}"]`)

  if (selectedElement) {
    selectedElement.classList.remove('selected')
  }

  if (elementToSelect) {
    elementToSelect.classList.add('selected')
  }
}

function newAutocompleteIndex (offset) {
  // Include the default first option with index -1
  const length = state.autocompleteSuggestions.length + 1
  const index = state.selectedIdx + offset
  return ((index + 1 + length) % length) - 1
}
