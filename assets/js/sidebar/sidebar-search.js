import { hideAutocompleteList, moveAutocompleteSelection, selectedAutocompleteSuggestion, updateAutocompleteList } from '../autocomplete/autocomplete-list'
import { qs } from '../helpers'

const SEARCH_FORM_SELECTOR = 'form.sidebar-search'
const SEARCH_INPUT_SELECTOR = 'form.sidebar-search input'

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
 * Focuses the search input.
 */
export function focusSearchInput () {
  const searchInput = qs(SEARCH_INPUT_SELECTOR)
  searchInput.focus()
}

function addEventListeners () {
  const searchInput = qs(SEARCH_INPUT_SELECTOR)

  searchInput.addEventListener('keydown', event => {
    if (event.key === 'Escape') {
      closeSearch()
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
      if (relatedTarget.classList.contains('autocomplete-suggestion')) {
        return null
      }

      if (relatedTarget.classList.contains('search-close-button')) {
        closeSearch()
      }
    }

    document.body.classList.remove('search-focused')
    hideAutocompleteList()
  })
}

function handleAutocompleteFormSubmission (event) {
  const searchForm = qs(SEARCH_FORM_SELECTOR)
  const searchInput = qs(SEARCH_INPUT_SELECTOR)
  const newWindowKeyDown = (event.shiftKey || event.ctrlKey)
  const autocompleteSuggestion = selectedAutocompleteSuggestion()

  event.preventDefault()

  const target = newWindowKeyDown ? '_blank' : '_self'
  searchForm.setAttribute('target', target)

  if (autocompleteSuggestion) {
    searchForm.setAttribute('action', autocompleteSuggestion.link)
    searchInput.removeAttribute('name')
  } else {
    searchForm.setAttribute('action', 'search.html')
    searchInput.setAttribute('name', 'q')
  }

  searchForm.submit()
}

function closeSearch () {
  const input = qs(SEARCH_INPUT_SELECTOR)
  input.value = ''
  input.blur()
}
