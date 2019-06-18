// quick switch modal

import quickSwitchModalTemplate from './templates/quick-switch-modal.handlebars'

// Constants
// ---------

const quickSwitchModalSelector = '#quick-switch-modal'
const closeButtonSelector = '.modal-close'

function showQuickSwichModal () {
  $(quickSwitchModalSelector).show().focus()
}

// Public Methods
// --------------

export { showQuickSwichModal }

export function initialize () {
  const quickSwitchModal = quickSwitchModalTemplate()
  $('body').append(quickSwitchModal)

  // TODO: Register elements
}
