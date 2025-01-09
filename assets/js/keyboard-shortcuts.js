import { isAppleOS, qs } from './helpers'
import { isSidebarOpened, openSidebar, toggleSidebar } from './sidebar/sidebar-drawer'
import { openVersionSelect } from './sidebar/sidebar-version-select'
import { focusSearchInput } from './search-bar'
import { cycleTheme } from './theme'
import { openQuickSwitchModal } from './quick-switch'
import { closeModal, isModalOpen } from './modal'
import { openSettingsModal } from './settings'
import { isEmbedded } from './globals'

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
    key: 'v',
    description: 'Open/focus version select',
    action: versionKeyAction
  },
  {
    key: 'g',
    description: 'Go to package docs',
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
 * Registers keyboard shortcuts.
 */
if (!isEmbedded) {
  document.addEventListener('keydown', handleKeyDown)
  document.addEventListener('keyup', handleKeyUp)
}

function handleKeyDown (event) {
  if (state.shortcutBeingPressed) { return }
  if (event.target.matches('input, select, textarea')) { return }

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

function versionKeyAction () {
  closeModal()

  if (isSidebarOpened()) {
    openVersionSelect()
  } else {
    openSidebar().then(openVersionSelect)
  }
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
