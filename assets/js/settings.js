import settingsModalBodyTemplate from './handlebars/templates/settings-modal-body.handlebars'
import { qs } from './helpers'
import { openModal } from './modal'
import { shouldUseNightMode } from './night'
import { settingsStore } from './settings-store'

const SETTINGS_LINK_SELECTOR = '.display-settings'
const SETTINGS_MODAL_BODY_SELECTOR = '#settings-modal-content'

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

function openSettingsModal () {
  openModal({
    title: 'Settings',
    body: settingsModalBodyTemplate({})
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
    } else {
      directLivebookUrlInput.checked = true
      livebookUrlInput.classList.remove('hidden')
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
}
