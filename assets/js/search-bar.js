import {
  hideAutocompleteList,
  isAutocompleteListOpen,
  moveAutocompleteSelection,
  selectedAutocompleteSuggestion,
  updateAutocompleteList,
  AUTOCOMPLETE_CONTAINER_SELECTOR,
  AUTOCOMPLETE_SUGGESTION_LIST_SELECTOR
} from './autocomplete/autocomplete-list'
import { isEmbedded } from './globals'
import { isAppleOS, qs } from './helpers'

const SEARCH_INPUT_SELECTOR = 'form.search-bar input'
const SEARCH_CLOSE_BUTTON_SELECTOR = 'form.search-bar .search-close-button'
const ENGINE_SELECTOR = '.engine-selector'
const ENGINE_BUTTON_SELECTOR = '.engine-button'
const ENGINE_DROPDOWN_SELECTOR = '.engine-dropdown'

// Search engine state - gets the currently selected engine URL from DOM
let selectedEngineUrl = null

/**
 * Initializes the sidebar search box.
 */

if (!isEmbedded) {
  window.addEventListener('exdoc:loaded', initialize)
}

function initialize () {
  initializeSearchEngineState()
  addEventListeners()
  setAutocompleteLimit()
}

function initializeSearchEngineState () {
  const allOptions = document.querySelectorAll('.engine-option')

  if (allOptions.length === 0) {
    selectedEngineUrl = 'search.html?q='
    return
  }

  // If we're on the search page, prefer the first engine that starts with search.html
  const pathname = window.location.pathname
  if (pathname.endsWith('/search.html') || pathname.endsWith('/search')) {
    const searchOption = Array.from(allOptions).find(option =>
      option.dataset.engineUrl.startsWith('search.html')
    )

    if (searchOption) {
      selectSearchEngine(searchOption)
      return
    }
  }

  // Otherwise, use the first option
  selectedEngineUrl = allOptions[0].dataset.engineUrl
}

function selectSearchEngine (option) {
  selectedEngineUrl = option.dataset.engineUrl

  // Update button text
  const button = qs(ENGINE_BUTTON_SELECTOR)
  const nameSpan = button?.querySelector('.engine-name')
  if (nameSpan) {
    nameSpan.textContent = option.querySelector('.name').textContent
  }

  // Update aria-checked attributes
  const dropdown = qs(ENGINE_DROPDOWN_SELECTOR)
  dropdown?.querySelectorAll('.engine-option').forEach(opt => {
    opt.setAttribute('aria-checked', opt === option ? 'true' : 'false')
  })
}

function toggleSearchEngineDropdown () {
  const button = qs(ENGINE_BUTTON_SELECTOR)
  const dropdown = qs(ENGINE_DROPDOWN_SELECTOR)
  if (!button || !dropdown) return

  const isExpanded = button.getAttribute('aria-expanded') === 'true'
  if (isExpanded) {
    hideSearchEngineDropdown()
  } else {
    showSearchEngineDropdown()
  }
}

function showSearchEngineDropdown () {
  const button = qs(ENGINE_BUTTON_SELECTOR)
  const dropdown = qs(ENGINE_DROPDOWN_SELECTOR)
  if (!button || !dropdown) return

  button.setAttribute('aria-expanded', 'true')
  dropdown.removeAttribute('hidden')

  // Focus first option
  const firstOption = dropdown.querySelector('.engine-option')
  firstOption?.focus()
}

function hideSearchEngineDropdown () {
  const button = qs(ENGINE_BUTTON_SELECTOR)
  const dropdown = qs(ENGINE_DROPDOWN_SELECTOR)
  if (!button || !dropdown) return

  button.setAttribute('aria-expanded', 'false')
  dropdown.setAttribute('hidden', '')
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
  // We also add the class before so we don't move the screen position
  document.body.classList.add('search-focused')
  searchInput.focus()
}

function setAutocompleteLimit () {
  const searchInput = qs(SEARCH_INPUT_SELECTOR)
  const autocompleteLimit = parseInt(document.querySelector('meta[name="exdoc:autocomplete-limit"]')?.content)
  if (autocompleteLimit) {
    window.autocompleteLimit = autocompleteLimit
  }
  searchInput.setAttribute('autocomplete-limit', autocompleteLimit)
}

function addEventListeners () {
  const searchInput = qs(SEARCH_INPUT_SELECTOR)

  if (document.querySelector('meta[name="exdoc:autocomplete"][content="off"]')) {
    searchInput.addEventListener('keydown', event => {
      if (event.key === 'Enter') {
        handleAutocompleteFormSubmission(event)
      }
    })

    return true
  }

  searchInput.addEventListener('keydown', event => {
    const appleOS = isAppleOS()

    if (event.key === 'Escape') {
      clearSearch()
      searchInput.blur()
    } else if (event.key === 'Enter') {
      handleAutocompleteFormSubmission(event)
    } else if (event.key === 'ArrowUp' || (appleOS && event.ctrlKey && event.key === 'p')) {
      moveAutocompleteSelection(-1)
      event.preventDefault()
    } else if (event.key === 'ArrowDown' || (appleOS && event.ctrlKey && event.key === 'n')) {
      moveAutocompleteSelection(1)
      event.preventDefault()
    }
  })

  searchInput.addEventListener('input', event => {
    updateAutocompleteList(event.target.value)
  })

  searchInput.addEventListener('focus', event => {
    if (!document.body.classList.contains('search-focused')) {
      document.body.classList.add('search-focused')
      updateAutocompleteList(event.target.value)
    }
  })

  searchInput.addEventListener('blur', event => {
    const relatedTarget = event.relatedTarget
    const suggestionList = qs(AUTOCOMPLETE_SUGGESTION_LIST_SELECTOR)

    if (relatedTarget && suggestionList && suggestionList.contains(relatedTarget)) {
      // If blur is triggered caused by clicking on an autocomplete result,
      // then ignore it, because it's handled in the click handler below.
      setTimeout(() => {
        // Focus the input after a while, so that it's easier to close
        // or get back to after an accidental blur
        if (isAutocompleteListOpen()) {
          searchInput.focus()
        }
      }, 1000)
      return null
    } else {
      hideAutocomplete()
    }
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

  qs(SEARCH_CLOSE_BUTTON_SELECTOR).addEventListener('click', _event => {
    clearSearch()
    hideAutocomplete()
  })

  // Search engine dropdown events
  const engineButton = qs(ENGINE_BUTTON_SELECTOR)
  if (engineButton) {
    engineButton.addEventListener('click', event => {
      event.stopPropagation()
      toggleSearchEngineDropdown()
    })

    engineButton.addEventListener('keydown', event => {
      if (event.key === 'Enter' || event.key === ' ') {
        event.preventDefault()
        toggleSearchEngineDropdown()
      } else if (event.key === 'Escape') {
        hideSearchEngineDropdown()
      }
    })
  }

  // Close dropdown when clicking outside
  document.addEventListener('click', event => {
    const selector = qs(ENGINE_SELECTOR)
    if (selector && !selector.contains(event.target)) {
      hideSearchEngineDropdown()
    }
  })

  // Add click handlers to engine options (rendered by Elixir)
  const dropdown = qs(ENGINE_DROPDOWN_SELECTOR)
  if (dropdown) {
    dropdown.querySelectorAll('.engine-option').forEach(option => {
      option.addEventListener('click', () => {
        selectSearchEngine(option)
        hideSearchEngineDropdown()
      })
    })

    // Keyboard navigation in dropdown
    dropdown.addEventListener('keydown', event => {
      const options = Array.from(dropdown.querySelectorAll('.engine-option'))
      const currentIndex = options.indexOf(document.activeElement)

      if (event.key === 'ArrowDown') {
        event.preventDefault()
        const nextIndex = (currentIndex + 1) % options.length
        options[nextIndex]?.focus()
      } else if (event.key === 'ArrowUp') {
        event.preventDefault()
        const prevIndex = (currentIndex - 1 + options.length) % options.length
        options[prevIndex]?.focus()
      } else if (event.key === 'Escape') {
        event.preventDefault()
        hideSearchEngineDropdown()
        engineButton?.focus()
      } else if (event.key === 'Enter' || event.key === ' ') {
        event.preventDefault()
        document.activeElement?.click()
      }
    })
  }
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
    // Use selected search engine URL
    anchor.setAttribute('href', `${selectedEngineUrl}${encodeURIComponent(searchInput.value)}`)
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
}

let lastScroll
const scrollIntentThreshold = 2

window.addEventListener('scroll', function () {
  const currentScroll = window.scrollY

  if (lastScroll !== undefined) {
    const diff = currentScroll - lastScroll
    if (currentScroll === 0 || diff > scrollIntentThreshold) {
      // Remove when at the top or scrolling down.
      document.body.classList.remove('scroll-sticky')
    } else if (currentScroll > 0 && -diff > scrollIntentThreshold) {
      // Add when scrolling up.
      document.body.classList.add('scroll-sticky')
    }
  }

  lastScroll = Math.max(0, currentScroll)
}, false)
