// quick switch modal

// Dependencies
// ------------

import $ from 'jquery'
import quickSwitchModalTemplate from './templates/quick-switch-modal.handlebars'
import quickSwitchResultsTemplate from './templates/quick-switch-results.handlebars'

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
 * @param {Object} e Keybord shortcut press event
 */
function openQuickSwitchModal (e) {
  $(quickSwitchModalSelector).show()
  $(quickSwitchInputSelector).focus()
  event.preventDefault()
}

/**
 * Closes the quick switch modal dialog.
 */
function closeQuickSwitchModal () {
  debounceTimeout = null
  autoCompleteResults = []
  autoCompleteSelected = -1

  $(quickSwitchInputSelector).blur()
  $(quickSwitchResultsSelector).html('')
  $(quickSwitchInputSelector).val('').removeClass('completed')
  $(quickSwitchModalSelector).hide()
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
function debouceAutocomplete (packageSlug) {
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
  $.get(hexSearchEndpoint.replace('%%', packageSlug), (payload) => {
    if (Array.isArray(payload)) {
      autoCompleteResults = resultsFromPayload(packageSlug, payload)
      autoCompleteSelected = -1

      const template = quickSwitchResultsTemplate({
        results: autoCompleteResults
      })

      // Only append results if string is still long enough
      const currentTerm = $(quickSwitchInputSelector).val()
      if (currentTerm && currentTerm.length >= 3) {
        $(quickSwitchResultsSelector).html(template)
        $(quickSwitchResultSelector).click(function () {
          const selectedResult = autoCompleteResults[$(this).attr('data-index')]
          switchToExDocPackage(selectedResult.name)
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
 * @param {Object} e The keypress event that triggered the moving
 * @param {String} updown Whether to move up or down the list
 */
function moveAutocompleteSelection (e, updown) {
  const selectedElement = $('.quick-switch-result.selected')

  if (selectedElement.length !== 0) {
    if (updown === 'up') {
      selectPrevAcResult(selectedElement)
    } else {
      selectNextAcResult(selectedElement)
    }
  } else {
    selectFirstAcResult()
  }

  e.preventDefault()
}

/**
 * Select the first result in the autocomplete result list.
 */
function selectFirstAcResult () {
  $(quickSwitchResultSelector).first().addClass('selected')
  autoCompleteSelected = 0
}

/**
 * Select the last result in the autocomplete result list.
 */
function selectLastAcResult () {
  $(quickSwitchResultSelector).last().addClass('selected')
  autoCompleteSelected = numberOfSuggestions
}

/**
 * Select next autocomplete result.
 * If the end of the list is reached, the first element is selected instead.
 *
 * @param {(Object|null)} selectedElement jQuery element of selected autocomplete result
 */
function selectNextAcResult (selectedElement) {
  const nextResult = selectedElement.next()
  selectedElement.removeClass('selected')

  if (nextResult.length !== 0) {
    nextResult.addClass('selected')
    autoCompleteSelected += 1
  } else {
    selectFirstAcResult()
  }
}

/**
 * Select previous autocomplete result.
 * If the beginning of the list is reached, the last element is selected instead.
 *
 * @param {(Object|null)} selectedElement jQuery element of selected autocomplete result
 */
function selectPrevAcResult (selectedElement) {
  const prevResult = selectedElement.prev()
  selectedElement.removeClass('selected')

  if (prevResult.length !== 0) {
    prevResult.addClass('selected')
    autoCompleteSelected -= 1
  } else {
    selectLastAcResult()
  }
}

/**
 * De-select the currently selected autocomplete result
 */
function deselectAcResult () {
  $('.quick-switch-result.selected').removeClass('selected')
  autoCompleteSelected = -1
}

// Public Methods
// --------------

export { openQuickSwitchModal }

export function initialize () {
  const quickSwitchModal = quickSwitchModalTemplate()
  $('body').append(quickSwitchModal)

  $(quickSwitchLinkSelector).click(openQuickSwitchModal)

  $(quickSwitchModalSelector).on('keydown', function (e) {
    if (e.keyCode === 27) { // escape key
      closeQuickSwitchModal()
    }
  })

  $(quickSwitchModalSelector).on('click', closeButtonSelector, function () {
    closeQuickSwitchModal()
  })

  $(quickSwitchInputSelector).on('keydown', function (e) {
    const packageSlug = $(quickSwitchInputSelector).val()

    if (e.keyCode === 13) { // enter key
      quickSwitchToPackage(packageSlug)
    }

    if (e.keyCode === 37 || e.keyCode === 39) deselectAcResult()
    if (e.keyCode === 38) moveAutocompleteSelection(e, 'up')
    if (e.keyCode === 40) moveAutocompleteSelection(e, 'down')
  })

  $(quickSwitchInputSelector).on('keyup', function (e) {
    const packageSlug = $(quickSwitchInputSelector).val() || ''

    if (e.keyCode === 8 && packageSlug.length < 3) {
      $(quickSwitchResultsSelector).html('')
    }

    if (usedModifierKeys.indexOf(e.keyCode) === -1 && packageSlug.length >= 3) {
      debouceAutocomplete(packageSlug)
    }
  })
}
