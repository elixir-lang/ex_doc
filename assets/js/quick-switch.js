// quick switch modal

import quickSwitchModalTemplate from './templates/quick-switch-modal.handlebars'

// Constants
// ---------

const quickSwitchModalSelector = '#quick-switch-modal'
const quickSwitchInputSelector = '#quick-switch-input'
const closeButtonSelector = '.modal-close'

function openQuickSwichModal (e) {
  $(quickSwitchModalSelector).show()
  $(quickSwitchInputSelector).focus().val("")
  event.preventDefault()
}

function closeQuickSwitchModal () {
  console.log('hiding')
  $(quickSwitchModalSelector).hide()
}

function quickSwitchToPackage (packageSlug) {
  window.location = `https://hexdocs.pm/${packageSlug}`
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

  $(quickSwitchInputSelector).on('keydown', function (e) {
    if (e.keyCode === 13) { // enter key
      const packageSlug = $(quickSwitchInputSelector).val()
      quickSwitchToPackage(packageSlug)
    }
  })
}
