import { qs } from './helpers'
import { openSidebar, toggleSidebar } from './sidebar/sidebar-drawer'
import { openQuickSearchModal } from './sidebar/quick-search'
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
    description: 'Open Quick Search modal',
    displayAs: '<kbd><kbd>/</kbd></kbd> or <kbd><kbd>s</kdb></kdb>',
    action: searchKeyAction
  },
  {
    key: 'k',
    handler: quickSearchShortcutHandler,
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
    description: 'Bring up this help dialog',
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

  const matchingShortcut = keyboardShortcuts.find(shortcut =>
      shortcut.handler === undefined ?
          shortcut.key === event.key && !event.ctrlKey && !event.metaKey && !event.altKey :
          shortcut.handler(event, shortcut)
  )
  if (!matchingShortcut) { return }
  state.shortcutBeingPressed = matchingShortcut

  event.preventDefault()
  matchingShortcut.action(event)
}

function handleKeyUp (event) {
  state.shortcutBeingPressed = null
}

function quickSearchShortcutHandler(event, shortcut) {
  return (event.ctrlKey || event.metaKey) && event.key === shortcut.key
}

// Additional shortcut actions

function searchKeyAction (event) {
  closeModal()
  openSidebar()
    .then(() => {
      openQuickSearchModal()
    })
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
