import { isAppleOS, qs } from './helpers'
import { toggleSidebar } from './sidebar/sidebar-drawer'
import { focusSearchInput } from './search-bar'
import { cycleTheme } from './theme'
import { openQuickSwitchModal } from './quick-switch'
import { closeModal, isModalOpen } from './modal'
import { openSettingsModal } from './settings'

const HELP_MODAL_BODY_SELECTOR = '#settings-modal-content'

export const keyboardShortcuts = [
  {
    key: 'c',
    description: 'Toggle sidebar',
    action: toggleSidebar
  },
  {
    key: 'n',
    description: 'Cycle themes',
    action: cycleTheme
  },
  {
    key: 's',
    description: 'Focus search bar',
    displayAs: '<kbd><kbd>/</kbd></kbd> or <kbd><kbd>s</kdb></kdb>',
    action: searchKeyAction
  },
  {
    key: '/',
    action: searchKeyAction
  },
  {
    key: 'k',
    hasModifier: true,
    action: searchKeyAction
  },
  {
    key: 'g',
    description: 'Search HexDocs package',
    displayAs: '<kbd><kbd>g</kdb></kdb>',
    action: openQuickSwitchModal
  },
  {
    key: '?',
    displayAs: '<kbd><kbd>?</kbd></kbd>',
    description: 'Bring up this modal',
    action: toggleHelpModal
  }
]

const state = {
  // Stores shortcut info to prevent multiple activations on long press (continuous keydown events)
  shortcutBeingPressed: null
}

/**
 * Registers keyboard shortcuts and sets up a help modal
 * listing all available options.
 */
export function initialize () {
  addEventListeners()
}

function addEventListeners () {
  document.addEventListener('keydown', handleKeyDown)
  document.addEventListener('keyup', handleKeyUp)
}

function handleKeyDown (event) {
  if (state.shortcutBeingPressed) { return }
  if (event.target.matches('input, textarea')) { return }

  const matchingShortcut = keyboardShortcuts.find(shortcut => {
    if (shortcut.hasModifier) {
      if (isAppleOS() && event.metaKey) { return shortcut.key === event.key }
      if (event.ctrlKey) { return shortcut.key === event.key }

      return false
    } else {
      if (event.ctrlKey || event.metaKey || event.altKey) { return false }

      return shortcut.key === event.key
    }
  })

  if (!matchingShortcut) { return }
  state.shortcutBeingPressed = matchingShortcut

  event.preventDefault()
  matchingShortcut.action(event)
}

function handleKeyUp (event) {
  state.shortcutBeingPressed = null
}

// Additional shortcut actions

function searchKeyAction (event) {
  closeModal()
  focusSearchInput()
}

function toggleHelpModal () {
  if (isHelpModalOpen()) {
    closeModal()
  } else {
    openSettingsModal()
  }
}

function isHelpModalOpen () {
  return isModalOpen() && qs(HELP_MODAL_BODY_SELECTOR)
}
