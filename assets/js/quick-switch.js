// quick switch modal

import $ from 'jquery'
import quickSwitchModalTemplate from './templates/quick-switch-modal.handlebars'
import quickSwitchResultsTemplate from './templates/quick-switch-results.handlebars'

// Constants
// ---------

const hexSearchEndpoint = 'https://hex.pm/api/packages?search=name:%%*'
const quickSwitchModalSelector = '#quick-switch-modal'
const quickSwitchInputSelector = '#quick-switch-input'
const quickSwitchResultSelector = '#quick-switch-results'
const closeButtonSelector = '.modal-close'
const debugKeypressTimeout = 300
let debounceTimeout = null

function openQuickSwichModal (e) {
  $(quickSwitchModalSelector).show()
  $(quickSwitchInputSelector).focus()
  event.preventDefault()
}

function closeQuickSwitchModal () {
  $(quickSwitchInputSelector).val('')
  $(quickSwitchModalSelector).hide()
}

function quickSwitchToPackage (packageSlug) {
  window.location = `https://hexdocs.pm/${packageSlug}`
}

function debouceAutocomplete (packageSlug) {
  if (!packageSlug || packageSlug.length < 3) return

  clearTimeout(debounceTimeout)
  debounceTimeout = setTimeout(() => {
    queryForAutocomplete(packageSlug)
  }, debugKeypressTimeout)
}

function queryForAutocomplete (packageSlug) {
  $.get(hexSearchEndpoint.replace('%%', packageSlug), (payload) => {
    if (Array.isArray(payload)) {
      const results = payload.slice(0, 9)
      const template = quickSwitchResultsTemplate({results})
      $(quickSwitchResultSelector).html(template);

      if (results.length > 0) {
        $(quickSwitchInputSelector).addClass('with-results')
      } else {
        $(quickSwitchInputSelector).removeClass('with-results')
      }
    }
  })
}

// Public Methods
// --------------

export { openQuickSwichModal }

export function initialize () {
  const quickSwitchModal = quickSwitchModalTemplate()
  $('body').append(quickSwitchModal)

  $(quickSwitchModalSelector).on('keydown', function (e) {
    if (e.keyCode === 27) { // escape key
      closeQuickSwitchModal()
    }
  })

  $(quickSwitchModalSelector).on('click', closeButtonSelector, function () {
    closeQuickSwitchModal()
  })

  $(quickSwitchInputSelector).on('keyup', function (e) {
    const packageSlug = $(quickSwitchInputSelector).val()

    if (e.keyCode === 13) { // enter key
      quickSwitchToPackage(packageSlug)
    } else {
      debouceAutocomplete(packageSlug)
    }
  })
}
