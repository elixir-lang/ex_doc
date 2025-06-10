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
  AUTOCOMPLETE_SUGGESTION_LIST_SELECTOR
} from './autocomplete/autocomplete-list'
import { isEmbedded, getSearchNodes, getVersionNodes } from './globals'
import { isAppleOS, qs } from './helpers'

const SEARCH_INPUT_SELECTOR = 'form.search-bar input'
const SEARCH_CLOSE_BUTTON_SELECTOR = 'form.search-bar .search-close-button'

/**
 * Initializes the sidebar search box.
 */

if (!isEmbedded) {
  window.addEventListener('exdoc:loaded', initialize)
}

function initialize () {
  addEventListeners()
  setAutocompleteLimit()

  window.onTogglePreviewClick = function (event, open) {
    event.preventDefault()
    event.stopImmediatePropagation()

    // Keep the focus on the input instead of the button
    // when the user clicked to open the preview.
    // Maintains consistent keyboard navigation and look
    focusSearchInput()
    if (open) { showPreview(event.target) } else { hidePreview() }
  }

  if (window.navigator.onLine) {
    enableRemoteSearch(null)
  } else {
    disableRemoteSearch(null)
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

function setAutocompleteLimit () {
  const searchInput = qs(SEARCH_INPUT_SELECTOR)
  const autocompleteLimit = parseInt(document.querySelector('meta[name="exdoc:autocomplete-limit"]').content)
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
    } else if (event.key === 'Tab' && selectedAutocompleteSuggestion() !== null) {
      togglePreview()
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

  window.addEventListener('online', enableRemoteSearch)
  window.addEventListener('offline', disableRemoteSearch)
}

function enableRemoteSearch (_event) {
  Array.from(document.getElementsByClassName('online-only')).forEach(element => {
    element.removeAttribute('disabled')
    element.removeAttribute('title')
    updateSearchTypeOptions(element)
  })
}

function disableRemoteSearch (_event) {
  Array.from(document.getElementsByClassName('online-only')).forEach(element => {
    element.setAttribute('disabled', '')
    element.setAttribute('title', 'Local searching only - browser is offline')
    updateSearchTypeOptions(element)
  })
}

function updateSearchTypeOptions (element) {
  Array.from(element.getElementsByTagName('option')).forEach(option => {
    if (option.value === 'related') {
      if (shouldEnableRelatedSearch()) {
        option.removeAttribute('disabled')
        option.removeAttribute('title')
      } else {
        option.setAttribute('disabled', '')
        option.setAttribute('title', 'No known related packages to search')
      }
    }
    if (option.value === 'latest') {
      if (shouldEnableLatestSearch()) {
        option.removeAttribute('disabled')
        option.removeAttribute('title')
      } else {
        option.setAttribute('disabled', '')
        option.setAttribute('title', 'Already browsing latest version')
      }
    }
  })
}

function shouldEnableRelatedSearch () {
  return getSearchNodes().length > 1
}

function shouldEnableLatestSearch () {
  const versionNodes = getVersionNodes()
  return versionNodes.length() > 0
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
    const url = meta ? meta.getAttribute('content') : 'search.html'

    const params = new URLSearchParams()
    params.set('q', searchInput.value)

    const searchType = getSearchType()

    if (searchType !== 'local') {
      params.set('type', searchType)
    }

    anchor.setAttribute('href', `${url}?${params.toString()}`)
  }

  anchor.click()

  if (!newWindowKeyDown) {
    clearSearch()
    hideAutocomplete()
  }
}

function getSearchType () {
  const searchTypes = Array.from(document.getElementsByClassName('search-type'))

  if (searchTypes.length > 0) {
    return searchTypes[0].value
  }

  return 'local'
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
