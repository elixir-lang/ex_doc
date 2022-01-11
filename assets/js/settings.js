import settingsModalBodyTemplate from './handlebars/templates/settings-modal-body.handlebars'
import { qs } from './helpers'
import { openModal } from './modal'
import { shouldUseNightMode } from './night'
import { settingsStore } from './settings-store'
import { keyboardShortcuts } from './keyboard-shortcuts'

const MODAL_TITLE_SELECTOR = '#modal .modal-title'
const SETTINGS_LINK_SELECTOR = '.display-settings'
const SETTINGS_MODAL_BODY_SELECTOR = '#settings-modal-content'
const SETTINGS_TAB = '#modal-settings-tab'
const KEYBOARD_SHORTCUTS_TAB = '#modal-keyboard-shortcuts-tab'
const SETTINGS_CONTENT = '#settings-content'
const KEYBOARD_SHORTCUTS_CONTENT = '#keyboard-shortcuts'

const modalTabs = [
  {
    title: 'Settings',
    id: 'modal-settings-tab'
  },
  {
    title: 'Keyboard Shortcuts',
    id: 'modal-keyboard-shortcuts-tab'
  }
]

/**
 * Sets up the settings modal.
 */
export function initialize () {
  addEventListeners()
}

function addEventListeners () {
  qs(SETTINGS_LINK_SELECTOR).addEventListener('click', event => {
    openSettingsModal()
  })
}

function showSettinsTab () {
  qs(KEYBOARD_SHORTCUTS_TAB).classList.remove('active')
  qs(SETTINGS_TAB).classList.add('active')
  qs(SETTINGS_CONTENT).classList.remove('hidden')
  qs(KEYBOARD_SHORTCUTS_CONTENT).classList.add('hidden')
}

function showKeyboardShortcutsTab () {
  qs(KEYBOARD_SHORTCUTS_TAB).classList.add('active')
  qs(SETTINGS_TAB).classList.remove('active')
  qs(KEYBOARD_SHORTCUTS_CONTENT).classList.remove('hidden')
  qs(SETTINGS_CONTENT).classList.add('hidden')
}

export function openSettingsModal () {
  openModal({
    title: 'Settings',
    body: settingsModalBodyTemplate({ tabs: modalTabs, shortcuts: keyboardShortcuts })
  })

  const modal = qs(SETTINGS_MODAL_BODY_SELECTOR)

  const nightModeInput = modal.querySelector(`[name="night_mode"]`)
  const tooltipsInput = modal.querySelector(`[name="tooltips"]`)
  const directLivebookUrlInput = modal.querySelector(`[name="direct_livebook_url"]`)
  const livebookUrlInput = modal.querySelector(`[name="livebook_url"]`)

  settingsStore.getAndSubscribe(settings => {
    nightModeInput.checked = shouldUseNightMode(settings)
    tooltipsInput.checked = settings.tooltips

    if (settings.livebookUrl === null) {
      directLivebookUrlInput.checked = false
      livebookUrlInput.classList.add('hidden')
      livebookUrlInput.tabIndex = -1
    } else {
      directLivebookUrlInput.checked = true
      livebookUrlInput.classList.remove('hidden')
      livebookUrlInput.tabIndex = 0
      livebookUrlInput.value = settings.livebookUrl
    }
  })

  nightModeInput.addEventListener('change', event => {
    settingsStore.update({ nightMode: event.target.checked })
  })

  tooltipsInput.addEventListener('change', event => {
    settingsStore.update({ tooltips: event.target.checked })
  })

  directLivebookUrlInput.addEventListener('change', event => {
    const livebookUrl = event.target.checked ? livebookUrlInput.value : null
    settingsStore.update({ livebookUrl })
  })

  livebookUrlInput.addEventListener('input', event => {
    settingsStore.update({ livebookUrl: event.target.value })
  })

  // Builds the navigation by tabs and hides the original modal title
  qs(MODAL_TITLE_SELECTOR).classList.add('sr-only')

  qs(SETTINGS_TAB).addEventListener('click', event => {
    showSettinsTab()
  })
  qs(KEYBOARD_SHORTCUTS_TAB).addEventListener('click', event => {
    showKeyboardShortcutsTab()
  })

  showSettinsTab()
}
