import {
  hideAutocompleteList,
  isAutocompleteListOpen,
  moveAutocompleteSelection,
  selectedAutocompleteSuggestion,
  togglePreview,
  showPreview,
  hidePreview,
  updateAutocompleteList,
  AUTOCOMPLETE_CONTAINER_SELECTOR,
  AUTOCOMPLETE_SUGGESTION_SELECTOR
} from './autocomplete/autocomplete-list'
import { isMacOS, qs } from './helpers'

const SEARCH_INPUT_SELECTOR = 'form.search-bar input'
const SEARCH_CLOSE_BUTTON_SELECTOR = 'form.search-bar .search-close-button'

/**
 * Initializes the sidebar search box.
 */
export function initialize () {
  addEventListeners()

  window.onTogglePreviewClick = function onTogglePreviewClick (event) {
    event.preventDefault()
    event.stopImmediatePropagation()

    // Keep the focus on the input instead of the button when the user clicked to open the preview
    // Maintains consistent keyboard navigation and look
    focusSearchInput()

    togglePreview()
  }
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
    const macOS = isMacOS()

    if (event.key === 'Escape') {
      clearSearch()
      searchInput.blur()
    } else if (event.key === 'Enter') {
      handleAutocompleteFormSubmission(event)
    } else if (event.key === 'ArrowUp' || (macOS && event.ctrlKey && event.key === 'p')) {
      moveAutocompleteSelection(-1)
      event.preventDefault()
    } else if (event.key === 'ArrowDown' || (macOS && event.ctrlKey && event.key === 'n')) {
      moveAutocompleteSelection(1)
      event.preventDefault()
    } else if (event.key === 'Tab') {
      togglePreview()
      event.preventDefault()
    } else if (event.key === 'ArrowRight') {
      showPreview()
      event.preventDefault()
    } else if (event.key === 'ArrowLeft') {
      hidePreview()
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
      } else {
        hideAutocomplete()
      }
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
    const meta = document.querySelector('meta[name="exdoc:full-text-search-url"]')
    const url = meta ? meta.getAttribute('content') : 'search.html?q='
    anchor.setAttribute('href', `${url}${encodeURIComponent(searchInput.value)}`)
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
  hidePreview()
  document.body.classList.remove('search-focused')
  hideAutocompleteList()
}

let lastScrollTop = window.scrollY
const scrollThreshold = 70 // Set a threshold for scroll, adjust as needed

window.addEventListener('scroll', function () {
  const currentScroll = window.scrollY

  // Add 'scroll-sticky' class when not at the top
  if (currentScroll > scrollThreshold * 2) {
    document.body.classList.add('scroll-sticky')
  }

  // Remove when at the top
  if (currentScroll === 0) {
    document.body.classList.remove('scroll-sticky')
  }

  if (currentScroll > lastScrollTop && currentScroll > scrollThreshold) {
    // Scrolling down and past the threshold
    document.body.classList.remove('scroll-sticky')
  } else {
    // Scrolling up or at the top of the page
    document.body.classList.add('scroll-sticky')
  }

  lastScrollTop = currentScroll <= 0 ? 0 : currentScroll
}, false)
