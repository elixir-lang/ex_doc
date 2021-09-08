import quickSwitchModalBodyTemplate from './handlebars/templates/quick-switch-modal-body.handlebars'
import quickSwitchResultsTemplate from './handlebars/templates/quick-switch-results.handlebars'
import { debounce, qs, qsAll } from './helpers'
import { openModal } from './modal'

const HEX_DOCS_ENDPOINT = 'https://hexdocs.pm/%%'
const HEX_SEARCH_ENDPOINT = 'https://hex.pm/api/packages?search=name:%%*'
const QUICK_SWITCH_LINK_SELECTOR = '.display-quick-switch'
const QUICK_SWITCH_INPUT_SELECTOR = '#quick-switch-input'
const QUICK_SWITCH_RESULTS_SELECTOR = '#quick-switch-results'
const QUICK_SWITCH_RESULT_SELECTOR = '.quick-switch-result'
const DEBOUNCE_KEYPRESS_TIMEOUT = 300
const NUMBER_OF_SUGGESTIONS = 9

// Core elixir packages to include in the autocomplete results
const STATIC_SEARCH_RESULTS = [
  'elixir',
  'eex',
  'ex_unit',
  'hex',
  'iex',
  'logger',
  'mix'
].map(name => ({ name }))

const state = {
  autocompleteResults: [],
  selectedIdx: null
}

/**
 * Initializes the quick switch modal.
 */
export function initialize () {
  addEventListeners()
}

function addEventListeners () {
  qs(QUICK_SWITCH_LINK_SELECTOR).addEventListener('click', event => {
    openQuickSwitchModal()
  })
}

function handleKeyDown (event) {
  const packageSlug = event.target.value

  if (event.key === 'Enter') {
    quickSwitchToPackage(packageSlug)
    event.preventDefault()
  } else if (event.key === 'ArrowUp') {
    moveAutocompleteSelection(-1)
    event.preventDefault()
  } else if (event.key === 'ArrowDown') {
    moveAutocompleteSelection(1)
    event.preventDefault()
  }
}

function handleInput (event) {
  const packageSlug = event.target.value

  if (packageSlug.length < 3) {
    const resultsContainer = qs(QUICK_SWITCH_RESULTS_SELECTOR)
    resultsContainer.innerHTML = ''
  } else {
    debouncedQueryForAutocomplete(packageSlug)
  }
}

/**
 * Opens the quick switch modal dialog.
 */
export function openQuickSwitchModal () {
  openModal({
    title: 'Go to a HexDocs package',
    body: quickSwitchModalBodyTemplate()
  })

  qs(QUICK_SWITCH_INPUT_SELECTOR).focus()

  const quickSwitchInput = qs(QUICK_SWITCH_INPUT_SELECTOR)
  quickSwitchInput.addEventListener('keydown', handleKeyDown)
  quickSwitchInput.addEventListener('input', handleInput)

  state.autocompleteResults = []
  state.selectedIdx = null
}

/**
 * Navigate to a package on HexDocs.
 * If an autocomplete entry is selected, it will be used instead of the input text.
 *
 * @param {String} packageSlug The searched package name
 */
function quickSwitchToPackage (packageSlug) {
  if (state.selectedIdx === null) {
    navigateToHexDocPackage(packageSlug)
  } else {
    const selectedResult = state.autocompleteResults[state.selectedIdx]
    navigateToHexDocPackage(selectedResult.name)
  }
}

/**
 * Navigates to HexDocs of a specific package.
 *
 * @param {String} packageSlug The package name to navigate to
 */
function navigateToHexDocPackage (packageSlug) {
  window.location = HEX_DOCS_ENDPOINT.replace('%%', packageSlug)
}

const debouncedQueryForAutocomplete = debounce(queryForAutocomplete, DEBOUNCE_KEYPRESS_TIMEOUT)

/**
 * Queries the HexDocs API for autocomplete results for a given package name.
 *
 * @param {String} packageSlug The searched package name
 */
function queryForAutocomplete (packageSlug) {
  const url = HEX_SEARCH_ENDPOINT.replace('%%', packageSlug)
  fetch(url)
    .then(response => response.json())
    .then(payload => {
      if (Array.isArray(payload)) {
        state.autocompleteResults = resultsFromPayload(packageSlug, payload)
        state.selectedIdx = null
        // Only render results if the search string is still long enough
        const currentTerm = qs(QUICK_SWITCH_INPUT_SELECTOR).value
        if (currentTerm.length >= 3) {
          renderResults({ results: state.autocompleteResults })
        }
      }
    })
}

function renderResults ({ results }) {
  const resultsContainer = qs(QUICK_SWITCH_RESULTS_SELECTOR)
  const resultsHtml = quickSwitchResultsTemplate({ results })
  resultsContainer.innerHTML = resultsHtml

  qsAll(QUICK_SWITCH_RESULT_SELECTOR).forEach(result => {
    result.addEventListener('click', event => {
      const index = result.getAttribute('data-index')
      const selectedResult = state.autocompleteResults[index]
      navigateToHexDocPackage(selectedResult.name)
    })
  })
}

/**
 * Extracts the first `NUMBER_OF_SUGGESTIONS` results from the payload response
 * and the hardcoded, available packages in `STATIC_SEARCH_RESULT`.
 * This also filters out packages that do not have their docs published on HexDocs.
 *
 * @param {String} packageSlug The searched package name
 * @param {Array} payload The payload returned by the search request
 */
function resultsFromPayload (packageSlug, payload) {
  return STATIC_SEARCH_RESULTS
    .concat(payload)
    .filter(result => result.name.includes(packageSlug))
    .filter(result => result.releases === undefined || result.releases[0]['has_docs'] === true)
    .slice(0, NUMBER_OF_SUGGESTIONS)
}

/**
 * Moves the autocomplete selection up or down.
 *
 * @param {Number} offset How much to move down (or up) the list.
 */
function moveAutocompleteSelection (offset) {
  state.selectedIdx = newAutocompleteIndex(offset)

  const selectedElement = qs('.quick-switch-result.selected')
  const elementToSelect = qs(`.quick-switch-result[data-index="${state.selectedIdx}"]`)

  if (selectedElement) {
    selectedElement.classList.remove('selected')
  }

  if (elementToSelect) {
    elementToSelect.classList.add('selected')
  }
}

function newAutocompleteIndex (offset) {
  const length = state.autocompleteResults.length

  if (state.selectedIdx === null) {
    if (offset >= 0) return 0
    if (offset < 0) return length - 1
  }

  const index = state.selectedIdx + offset
  return (index + length) % length
}
