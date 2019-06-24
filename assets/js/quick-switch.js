// quick switch modal

import $ from 'jquery'
import quickSwitchModalTemplate from './templates/quick-switch-modal.handlebars'
import quickSwitchResultsTemplate from './templates/quick-switch-results.handlebars'

// Constants
// ---------

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

// State
// -----
let debounceTimeout = null
let autoCompleteResults = []
let autoCompleteSelected = -1

function openQuickSwitchModal (e) {
  $(quickSwitchModalSelector).show()
  $(quickSwitchInputSelector).focus()
  event.preventDefault()
}

function closeQuickSwitchModal () {
  debounceTimeout = null
  autoCompleteResults = []
  autoCompleteSelected = -1

  $(quickSwitchResultsSelector).html('')
  $(quickSwitchInputSelector).val('').removeClass('completed')
  $(quickSwitchModalSelector).hide()
}

function quickSwitchToPackage (packageSlug) {
  if (autoCompleteSelected === -1) {
    switchToExDocPackage(packageSlug)
  } else {
    const selectedResult = autoCompleteResults[autoCompleteSelected]
    switchToExDocPackage(selectedResult.name)
  }
}

function switchToExDocPackage (packageSlug) {
  window.location = `https://hexdocs.pm/${packageSlug}`
}

function debouceAutocomplete (packageSlug) {
  clearTimeout(debounceTimeout)
  debounceTimeout = setTimeout(() => {
    queryForAutocomplete(packageSlug)
  }, debounceKeypressTimeout)
}

function queryForAutocomplete (packageSlug) {
  $.get(hexSearchEndpoint.replace('%%', packageSlug), (payload) => {
    if (Array.isArray(payload)) {
      autoCompleteResults = payload.slice(0, numberOfSuggestions)
      autoCompleteSelected = -1

      const template = quickSwitchResultsTemplate({
        results: autoCompleteResults
      })
      $(quickSwitchResultsSelector).html(template)
      setupAutocompleteListeners()

      if (autoCompleteResults.length > 0) {
        $(quickSwitchInputSelector).addClass('completed')
      } else {
        $(quickSwitchInputSelector).removeClass('completed')
      }
    }
  })
}

function setupAutocompleteListeners () {
  $(quickSwitchResultSelector).click(function () {
    const selectedResult = autoCompleteResults[$(this).attr('data-index')]
    switchToExDocPackage(selectedResult.name)
  })

  $(quickSwitchResultSelector).mouseenter(function () {
    autoCompleteSelected = $(this).attr('data-index')
    $(this).addClass('selected')
  })

  $(quickSwitchResultSelector).mouseleave(function () {
    autoCompleteSelected = -1
    $(this).removeClass('selected')
  })
}

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

function selectFirstAcResult () {
  $(quickSwitchResultSelector).first().addClass('selected')
  autoCompleteSelected = 0
}

function selectLastAcResult () {
  $(quickSwitchResultSelector).last().addClass('selected')
  autoCompleteSelected = numberOfSuggestions
}

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
