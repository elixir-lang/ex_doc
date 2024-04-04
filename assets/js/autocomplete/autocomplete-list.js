import { getSuggestions } from './suggestions'
import { isBlank, qs } from '../helpers'

export const AUTOCOMPLETE_CONTAINER_SELECTOR = '.autocomplete'
export const AUTOCOMPLETE_SUGGESTION_LIST_SELECTOR = '.autocomplete-suggestions'
export const AUTOCOMPLETE_SUGGESTION_SELECTOR = '.autocomplete-suggestion'

const state = {
  autocompleteSuggestions: [],
  previewOpen: false,
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
    showPreview(elementToSelect)

    elementToSelect.scrollIntoView({ block: 'nearest' })
  } else {
    const list = qs(AUTOCOMPLETE_SUGGESTION_LIST_SELECTOR)
    if (list) { list.scrollTop = 0 }
  }
}

/**
 * Toggles the preview state of the autocomplete list
 */
export function togglePreview () {
  state.previewOpen = !state.previewOpen
  const suggestionList = qs(AUTOCOMPLETE_SUGGESTION_LIST_SELECTOR)
  if (state.previewOpen) {
    suggestionList.classList.add('previewing')
    const elementToSelect = qs(`${AUTOCOMPLETE_SUGGESTION_SELECTOR}[data-index="${state.selectedIdx}"]`)
    showPreview(elementToSelect)
  } else {
    suggestionList.classList.remove('previewing')
  }
}

function expandPreview () {
  state.previewOpen = true
  const suggestionList = qs(AUTOCOMPLETE_SUGGESTION_LIST_SELECTOR)
  if (suggestionList) {
    suggestionList.classList.add('previewing')
  }
}

function closePreview () {
  state.previewOpen = false
  const suggestionList = qs(AUTOCOMPLETE_SUGGESTION_LIST_SELECTOR)
  if (suggestionList) {
    suggestionList.classList.remove('previewing')
  }
}

export function removePreview () {
  state.previewOpen = false
  const suggestionList = qs(AUTOCOMPLETE_SUGGESTION_LIST_SELECTOR)

  if (suggestionList) {
    suggestionList.classList.remove('previewing')
  }

  const container = previewContainer()

  if (container) {
    container.remove()
  }
}

function showPreview (elementToSelect) {
  const container = previewContainer()

  if (container) {
    container.remove()
  };

  const suggestionList = qs(AUTOCOMPLETE_SUGGESTION_LIST_SELECTOR)

  if (state.previewOpen && elementToSelect) {
    suggestionList.classList.add('previewing')
    const newContainer = document.createElement('div')
    newContainer.classList.add('autocomplete-preview')
    const iframe = document.createElement('iframe')
    const previewHref = elementToSelect.href.replace('.html', '.html?preview=true')
    // The minimum permissions necessary for the iframe to run JavaScript and communicate with the parent window.
    iframe.setAttribute('sandbox', 'allow-scripts allow-same-origin allow-popups')
    iframe.setAttribute('src', previewHref)
    newContainer.replaceChildren(iframe)
    iframe.onload = () => {
      if (iframe.contentDocument) {
        iframe.contentDocument.addEventListener('keydown', event => {
          if (event.key === 'ArrowLeft') {
            closePreview()
            event.preventDefault()
          } else if (event.key === 'ArrowRight') {
            expandPreview()
            event.preventDefault()
          }
        })
      }
    }
    elementToSelect.parentNode.insertBefore(newContainer, elementToSelect.nextSibling)
  } else {
    suggestionList.classList.remove('previewing')
  }
}

function previewContainer () {
  return qs('.autocomplete-preview')
}

function newAutocompleteIndex (offset) {
  // Include the default first option with index -1
  const length = state.autocompleteSuggestions.length + 1
  const index = state.selectedIdx + offset
  return ((index + 1 + length) % length) - 1
}
