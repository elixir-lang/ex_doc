import { settingsStore } from './settings-store'

const NIGHT_MODE_CLASS = 'night-mode'

/**
 * Sets initial night mode state and registers to settings updates.
 */
export function initialize () {
  settingsStore.getAndSubscribe(settings => {
    document.body.classList.toggle(NIGHT_MODE_CLASS, shouldUseNightMode(settings))
  })
}

/**
 * Toggles night mode theme and saves the preference.
 */
export function toggleNightMode () {
  const settings = settingsStore.get()
  settingsStore.update({ nightMode: !settings.nightMode })
}

export function shouldUseNightMode (settings) {
  return (settings.nightMode === true) ||
    (settings.nightMode === null && prefersDarkColorScheme())
}

function prefersDarkColorScheme () {
  return window.matchMedia('(prefers-color-scheme: dark)').matches
}
