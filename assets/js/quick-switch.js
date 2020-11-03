// quick switch modal

// Dependencies
// ------------

import quickSwitchModalTemplate from './templates/quick-switch-modal.handlebars'
import quickSwitchResultsTemplate from './templates/quick-switch-results.handlebars'
import {qs, qsAll} from './helpers'

// Constants
// ---------

const hexDocsEndpoint = 'https://hexdocs.pm/%%'
const hexSearchEndpoint = 'https://hex.pm/api/packages?search=name:%%*'
const quickSwitchLinkSelector = '.display-quick-switch'
const quickSwitchModalSelector = '#quick-switch-modal'
const quickSwitchInputSelector = '#quick-switch-input'
const quickSwitchResultsSelector = '#quick-switch-results'
const quickSwitchResultSelector = '.quick-switch-result'
const closeButtonSelector = '.modal-close'
const debounceKeypressTimeout = 300
const numberOfSuggestions = 9
const usedModifierKeys = [
  13, // enter
  27, // escape
  37, // left arrow
  38, // up arrow
  39, // right arrow
  40 // down arrow
]
const staticSearchResults = [
  'elixir',
  'eex',
  'ex_unit',
  'hex',
  'iex',
  'logger',
  'mix'
].map((pkg) => { return {name: pkg} })

// State
// -----

let debounceTimeout = null
let autoCompleteResults = []
let autoCompleteSelected = -1

/**
 * Opens the quick switch modal dialog.
 *
 * @param {Object} event Keybord shortcut press event
 */
function openQuickSwitchModal (event) {
  qs(quickSwitchModalSelector).classList.add('shown')
  qs(quickSwitchInputSelector).focus()
  event.preventDefault()
}

/**
 * Closes the quick switch modal dialog.
 */
function closeQuickSwitchModal () {
  debounceTimeout = null
  autoCompleteResults = []
  autoCompleteSelected = -1

  qs(quickSwitchModalSelector).classList.remove('shown')
  qs(quickSwitchInputSelector).blur()
  qs(quickSwitchInputSelector).value = ''
  qs(quickSwitchResultsSelector).innerHTML = ''
}

/**
 * Switch to a package on HexDocs.
 * If an autocomplete entry is selected, it will be used instead of the packageSlug.
 *
 * @param {String} packageSlug The searched package name
 */
function quickSwitchToPackage (packageSlug) {
  if (autoCompleteSelected === -1) {
    switchToExDocPackage(packageSlug)
  } else {
    const selectedResult = autoCompleteResults[autoCompleteSelected]
    switchToExDocPackage(selectedResult.name)
  }
}

/**
 * Navigates to HexDocs of a specific package.
 *
 * @param {String} packageSlug The package name to navigate to
 */
function switchToExDocPackage (packageSlug) {
  window.location = hexDocsEndpoint.replace('%%', packageSlug)
}

/**
 * Debounces API calls to HexDocs for autocompleting package names.
 *
 * @param {String} packageSlug The searched package name
 */
function debounceAutocomplete (packageSlug) {
  clearTimeout(debounceTimeout)
  debounceTimeout = setTimeout(() => {
    queryForAutocomplete(packageSlug)
  }, debounceKeypressTimeout)
}

/**
 * Queries the HexDocs API for autocomplete results for a given package name.
 *
 * @param {String} packageSlug The searched package name
 */
function queryForAutocomplete (packageSlug) {
  const url = hexSearchEndpoint.replace('%%', packageSlug)
  fetch(url)
    .then(response => response.json())
    .then(payload => {
      if (Array.isArray(payload)) {
        autoCompleteResults = resultsFromPayload(packageSlug, payload)
        autoCompleteSelected = -1

        const template = quickSwitchResultsTemplate({
          results: autoCompleteResults
        })

        // Only append results if string is still long enough
        const currentTerm = qs(quickSwitchInputSelector).value
        if (currentTerm && currentTerm.length >= 3) {
          qs(quickSwitchResultsSelector).innerHTML = template
          qsAll(quickSwitchResultSelector).forEach(quickSwitchResult => {
            quickSwitchResult.addEventListener('click', function () {
              const index = quickSwitchResult.getAttribute('data-index')
              const selectedResult = autoCompleteResults[index]
              switchToExDocPackage(selectedResult.name)
            })
          })
        }
      }
    })
}

/**
 * Extracts the first `numberOfSuggestions` results from the payload response
 * and the hardcoded, available packages in `staticSearchResults`.
 * This also filters out packages that do not have their docs published on HexDocs.
 *
 * @param {String} packageSlug The searched package name
 * @param {Array} payload The payload returned by the search request
 */
function resultsFromPayload (packageSlug, payload) {
  return staticSearchResults
    .concat(payload)
    .filter(result => result.name.indexOf(packageSlug) !== -1)
    .filter(result => result.releases === undefined || result.releases[0].has_docs === true)
    .slice(0, numberOfSuggestions)
}

/**
 * Moves the autocomplete selection up or down.
 *
 * @param {Object} event The keypress event that triggered the moving
 * @param {String} updown Whether to move up or down the list
 */
function moveAutocompleteSelection (event, updown) {
  const selectedElement = qs('.quick-switch-result.selected')

  if (selectedElement) {
    if (updown === 'up') {
      selectPrevAcResult(selectedElement)
    } else {
      selectNextAcResult(selectedElement)
    }
  } else {
    selectFirstAcResult()
  }

  event.preventDefault()
}

/**
 * Select the first result in the autocomplete result list.
 */
function selectFirstAcResult () {
  qs(quickSwitchResultSelector + ':first-child').classList.add('selected')
  autoCompleteSelected = 0
}

/**
 * Select the last result in the autocomplete result list.
 */
function selectLastAcResult () {
  qs(quickSwitchResultSelector + ':last-child').classList.add('selected')
  autoCompleteSelected = numberOfSuggestions
}

/**
 * Select next autocomplete result.
 * If the end of the list is reached, the first element is selected instead.
 *
 * @param {Object} selectedElement element of selected autocomplete result
 */
function selectNextAcResult (selectedElement) {
  const nextResult = selectedElement.nextElementSibling
  selectedElement.classList.remove('selected')

  if (nextResult) {
    nextResult.classList.add('selected')
    autoCompleteSelected += 1
  } else {
    selectFirstAcResult()
  }
}

/**
 * Select previous autocomplete result.
 * If the beginning of the list is reached, the last element is selected instead.
 *
 * @param {Object} selectedElement element of selected autocomplete result
 */
function selectPrevAcResult (selectedElement) {
  const prevResult = selectedElement.previousElementSibling
  selectedElement.classList.remove('selected')

  if (prevResult) {
    prevResult.classList.add('selected')
    autoCompleteSelected -= 1
  } else {
    selectLastAcResult()
  }
}

/**
 * De-select the currently selected autocomplete result
 */
function deselectAcResult () {
  const selectedElement = qs('.quick-switch-result.selected')
  if (selectedElement) {
    selectedElement.classList.remove('selected')
    autoCompleteSelected = -1
  }
}

// Public Methods
// --------------

export { openQuickSwitchModal }

export function initialize () {
  const quickSwitchModal = quickSwitchModalTemplate()
  qs('body').insertAdjacentHTML('beforeend', quickSwitchModal)

  qs(quickSwitchLinkSelector).addEventListener('click', openQuickSwitchModal)

  qs(quickSwitchModalSelector).addEventListener('keydown', function (event) {
    if (event.keyCode === 27) { // escape key
      closeQuickSwitchModal()
    }
  })

  qs(quickSwitchModalSelector).addEventListener('click', function (event) {
    if (event.target === qs(closeButtonSelector)) {
      closeQuickSwitchModal()
    }
  })

  qs(quickSwitchInputSelector).addEventListener('keydown', function (event) {
    const packageSlug = event.target.value

    if (event.keyCode === 13) { // enter key
      quickSwitchToPackage(packageSlug)
    }

    if (event.keyCode === 37 || event.keyCode === 39) deselectAcResult()
    if (event.keyCode === 38) moveAutocompleteSelection(event, 'up')
    if (event.keyCode === 40) moveAutocompleteSelection(event, 'down')
  })

  qs(quickSwitchInputSelector).addEventListener('keyup', function (event) {
    const packageSlug = qs(quickSwitchInputSelector).value

    if (event.keyCode === 8 && packageSlug.length < 3) {
      qs(quickSwitchResultsSelector).innerHTML = ''
    }

    if (usedModifierKeys.indexOf(event.keyCode) === -1 && packageSlug.length >= 3) {
      debounceAutocomplete(packageSlug)
    }
  })
}
