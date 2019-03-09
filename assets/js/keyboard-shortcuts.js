// Dependencies
// ------------

import $ from 'jquery'
import find from 'lodash.find'
import {focusSearchInput, openSidebar, toggleSidebar} from './sidebar'
import {toggleNightMode} from './night'
import helpModalTemplate from './templates/keyboard-shortcuts-help-modal.handlebars'

// Constants
// ---------

const helpModalSelector = '#keyboard-shortcuts-modal'
const closeButtonSelector = '.modal-close'
const inputElements = ['input', 'textarea']
const keyboardShortcuts = [
  {
    name: 'c',
    keyCode: 67,
    description: 'Toggle sidebar',
    action: toggleSidebar
  },
  {
    name: 'n',
    keyCode: 78,
    description: 'Toggle night mode',
    action: toggleNightMode,
    eventType: 'keyup'
  },
  {
    name: 's',
    keyCode: 83,
    description: 'Focus search bar',
    displayAs: '<kbd>/</kbd> or <kbd>s</kdb>',
    action: searchKeyAction
  },
  {
    name: '/',
    keyCode: 191,
    action: searchKeyAction
  },
  {
    name: '?',
    keyCode: 191,
    requiresShiftKey: true,
    displayAs: '<kbd>shift</kbd> + <kbd>/</kbd>',
    description: 'Bring up this help dialog',
    action: toggleHelpModal
  }
]

// Local Methods
// ---------------

function triggerShortcut (event) {
  const elementTagName = event.target.tagName.toLowerCase()
  const keyCode = event.keyCode
  const isShiftPressed = event.shiftKey
  const eventType = event.type

  if (inputElements.indexOf(elementTagName) >= 0) { return }

  if (event.ctrlKey || event.metaKey || event.altKey) { return }

  const foundShortcut = find(keyboardShortcuts, function (shortcut) {
    const isShiftRequired = !!shortcut.requiresShiftKey
    return shortcut.keyCode === keyCode && isShiftRequired === isShiftPressed
  })

  if (!foundShortcut) { return }

  const shortcutEventType = foundShortcut.eventType || 'keydown'

  if (shortcutEventType !== eventType) { return }

  foundShortcut.action(event)
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

function searchKeyAction () {
  openSidebar()
  closeHelpModal()
  focusSearchInput()
  event.preventDefault()
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

  $(document).on('keyup', function (e) {
    triggerShortcut(e)
  })
}
