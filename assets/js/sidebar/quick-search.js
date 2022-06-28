import {
  hideAutocompleteList,
  isAutocompleteListOpen,
  moveAutocompleteSelection,
  selectedAutocompleteSuggestion,
  updateAutocompleteList,
  AUTOCOMPLETE_CONTAINER_SELECTOR,
  AUTOCOMPLETE_SUGGESTION_SELECTOR
} from '../autocomplete/autocomplete-list'
import { qs } from '../helpers'

const SEARCH_INPUT_SELECTOR = '.quick-search input'
const SEARCH_BUTTON_SELECTOR = '.search-button'
const QUICK_SEARCH_MODAL_SELECTOR = '.quick-search'

/**
 * Initializes the sidebar search box.
 */
export function initialize () {
  addEventListeners()
}

/**
 * Sets the search input value.
 *
 * @param {String} value
 */
export function setSearchInputValue (value) {
  const searchInput = qs(SEARCH_INPUT_SELECTOR)
  searchInput.value = value
}

/**
 * Open Quick Search modal and focuses the search input.
 */
export function openQuickSearchModal () {
  showQuickSearchModal()

  const searchInput = qs(SEARCH_INPUT_SELECTOR)
  searchInput.focus()
}

function addEventListeners () {
  const searchInput = qs(SEARCH_INPUT_SELECTOR)

  searchInput.addEventListener('keydown', event => {
    if (event.key === 'Escape') {
      clearSearch()
      searchInput.blur()
    } else if (event.key === 'Enter') {
      handleAutocompleteFormSubmission(event)
    } else if (event.key === 'ArrowUp') {
      moveAutocompleteSelection(-1)
      event.preventDefault()
    } else if (event.key === 'ArrowDown') {
      moveAutocompleteSelection(1)
      event.preventDefault()
    }
  })

  searchInput.addEventListener('input', event => {
    updateAutocompleteList(event.target.value)
  })

  searchInput.addEventListener('focus', event => {
    document.body.classList.add('search-focused')
    updateAutocompleteList(event.target.value)
  })

  searchInput.addEventListener('blur', event => {
    const relatedTarget = event.relatedTarget

    if (relatedTarget) {
      // If blur is triggered caused by clicking on an autocomplete result,
      // then ignore it, because it's handled in the click handler below.
      if (relatedTarget.matches(AUTOCOMPLETE_SUGGESTION_SELECTOR)) {
        // Focus the input after a while, so that it's easier to close
        // or get back to after an accidental blur
        setTimeout(() => {
          if (isAutocompleteListOpen()) {
            searchInput.focus()
          }
        }, 1000)
        return null
      }
    }

    hideAutocomplete()
  })

  qs(AUTOCOMPLETE_CONTAINER_SELECTOR).addEventListener('click', event => {
    const newWindowKeyDown = (event.shiftKey || event.ctrlKey)
    if (newWindowKeyDown) {
      searchInput.focus()
    } else {
      clearSearch()
      hideAutocomplete()
    }
  })

  qs(SEARCH_BUTTON_SELECTOR).addEventListener('click', event => {
    showQuickSearchModal()
    searchInput.focus()
  })
}

function handleAutocompleteFormSubmission (event) {
  const searchInput = qs(SEARCH_INPUT_SELECTOR)
  const newWindowKeyDown = (event.shiftKey || event.ctrlKey)
  const autocompleteSuggestion = selectedAutocompleteSuggestion()

  event.preventDefault()

  const target = newWindowKeyDown ? '_blank' : '_self'
  const anchor = document.createElement('a')

  anchor.setAttribute('target', target)

  if (autocompleteSuggestion) {
    anchor.setAttribute('href', autocompleteSuggestion.link)
  } else {
    anchor.setAttribute('href', `search.html?q=${encodeURIComponent(searchInput.value)}`)
  }

  anchor.click()

  if (!newWindowKeyDown) {
    clearSearch()
    hideAutocomplete()
  }
}

function clearSearch () {
  const input = qs(SEARCH_INPUT_SELECTOR)
  input.value = ''
}

function hideAutocomplete () {
  document.body.classList.remove('search-focused')
  hideAutocompleteList()
  hideQuickSearchModal()
}

function showQuickSearchModal () {
  qs(QUICK_SEARCH_MODAL_SELECTOR).classList.add('shown')
}

function hideQuickSearchModal () {
  qs(QUICK_SEARCH_MODAL_SELECTOR).classList.remove('shown')
}