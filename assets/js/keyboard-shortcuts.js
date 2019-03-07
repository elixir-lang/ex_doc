// Dependencies
// ------------

import $ from 'jquery'
import {focusSearchInput, openSidebar, toggleSidebar} from './sidebar'
import {toggleNightMode} from './night'
import helpModalTemplate from './templates/keyboard-shortcuts-help-modal.handlebars'

// Constants
// ---------

const helpModalSelector = '#keyboard-shortcuts-modal'
const closeButtonSelector = '.modal-close'
const inputElements = ['input', 'textarea']
const keyboardShortcuts = {
  67: {
    name: 'c',
    description: 'Toggle sidebar',
    action: toggleSidebar
  },
  78: {
    name: 'n',
    description: 'Toggle night mode',
    action: toggleNightMode
  },
  83: {
    name: 's',
    description: 'Focus search bar',
    action: (event) => {
      openSidebar()
      closeHelpModal()
      focusSearchInput()
      event.preventDefault()
    }
  },
  191: {
    name: '/',
    description: 'Bring up this help dialog',
    requiresShiftKey: true,
    action: toggleHelpModal
  }
}

// Local Methods
// ---------------

function triggerShortcut (event) {
  const elementTagName = event.target.tagName.toLowerCase()
  const keyCode = event.keyCode
  const isShiftPressed = event.shiftKey

  if (inputElements.indexOf(elementTagName) >= 0) { return }

  const shortcut = keyboardShortcuts[keyCode]
  if (!shortcut) { return true }

  const isShiftRequired = !!shortcut.requiresShiftKey
  if (isShiftRequired !== isShiftPressed) { return }

  shortcut.action(event)
}

function closeHelpModal () {
  $(helpModalSelector).hide()
}

function openHelpModal () {
  $(helpModalSelector).show().focus()
}

function toggleHelpModal () {
  if ($(helpModalSelector).is(':visible')) {
    closeHelpModal()
  } else {
    openHelpModal()
  }
}

// Public Methods
// --------------

export function initialize () {
  const helpModal = helpModalTemplate({shortcuts: keyboardShortcuts})
  $('body').append(helpModal)

  $(helpModalSelector).on('keydown', function (e) {
    if (e.keyCode === 27) { // escape key
      closeHelpModal()
    }
  })

  $(helpModalSelector).on('click', closeButtonSelector, function () {
    closeHelpModal()
  })

  $(document).on('keydown', function (e) {
    triggerShortcut(e)
  })
}
