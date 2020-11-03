// Dependencies
// ------------

import find from 'lodash.find'
import {focusSearchInput, openSidebar, toggleSidebar} from './sidebar'
import {toggleNightMode} from './night'
import {openQuickSwitchModal} from './quick-switch'
import helpModalTemplate from './templates/keyboard-shortcuts-help-modal.handlebars'
import {qs} from './helpers'

// Constants
// ---------

const helpModalSelector = '#keyboard-shortcuts-modal'
const closeButtonSelector = '.modal-close'
const helpLinkSelector = '.display-shortcuts-help'
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
    action: toggleNightMode
  },
  {
    name: 's',
    keyCode: 83,
    description: 'Focus search bar',
    displayAs: '<kbd><kbd>/</kbd></kbd> or <kbd><kbd>s</kdb></kdb>',
    action: searchKeyAction
  },
  {
    name: '/',
    keyCode: 191,
    action: searchKeyAction
  },
  {
    name: 'g',
    keyCode: 71,
    description: 'Go to a HexDocs package',
    displayAs: '<kbd><kbd>g</kdb></kdb>',
    action: openQuickSwitchModal
  },
  {
    name: '?',
    keyCode: 191,
    requiresShiftKey: true,
    displayAs: '<kbd><kbd>?</kbd></kbd>',
    description: 'Bring up this help dialog',
    action: toggleHelpModal
  }
]

// State
// -----

// Stores shortcut info to prevent multiple activations on keyDown event
let shortcutBeingPressed = null

// Local Methods
// ---------------

function triggerShortcut (event) {
  const elementTagName = event.target.tagName.toLowerCase()
  const key = event.key
  const keyCode = event.keyCode
  const isShiftPressed = event.shiftKey

  if (shortcutBeingPressed) { return }

  if (inputElements.indexOf(elementTagName) >= 0) { return }

  if (event.ctrlKey || event.metaKey || event.altKey) { return }

  const foundShortcut = find(keyboardShortcuts, function (shortcut) {
    if (key) return shortcut.name === key

    // legacy fallback
    const isShiftRequired = !!shortcut.requiresShiftKey
    return shortcut.keyCode === keyCode && isShiftRequired === isShiftPressed
  })

  if (!foundShortcut) { return }

  shortcutBeingPressed = foundShortcut

  foundShortcut.action(event)
}

function closeHelpModal () {
  qs(helpModalSelector).classList.remove('shown')
}

function openHelpModal () {
  qs(helpModalSelector).classList.add('shown')
  qs(helpModalSelector).focus()
}

function toggleHelpModal () {
  if (qs(helpModalSelector).classList.contains('shown')) {
    closeHelpModal()
  } else {
    openHelpModal()
  }
}

function searchKeyAction (event) {
  openSidebar()
  closeHelpModal()
  focusSearchInput()
  event.preventDefault()
}

// Public Methods
// --------------

export function initialize () {
  const helpModal = helpModalTemplate({shortcuts: keyboardShortcuts})
  qs('body').insertAdjacentHTML('beforeend', helpModal)

  qs(helpModalSelector).addEventListener('keydown', function (event) {
    if (event.keyCode === 27) { // escape key
      closeHelpModal()
    }
  })

  qs(helpModalSelector).addEventListener('click', function (event) {
    if (event.target === qs(closeButtonSelector)) {
      closeHelpModal()
    }
  })

  qs(helpLinkSelector).addEventListener('click', function (event) {
    openHelpModal()
  })

  document.addEventListener('keydown', function (event) {
    triggerShortcut(event)
  })

  document.addEventListener('keyup', function (event) {
    shortcutBeingPressed = null
  })
}
