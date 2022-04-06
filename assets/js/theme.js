import { settingsStore } from './settings-store'
import { showToast } from './toast'

const DARK_MODE_CLASS = 'dark'
const THEMES = ['system', 'dark', 'light']

/**
 * Sets initial night mode state and registers to settings updates.
 */
export function initialize () {
  settingsStore.getAndSubscribe(settings => {
    document.body.classList.toggle(DARK_MODE_CLASS, shouldUseDarkMode(settings))
  })
  listenToDarkMode()
}

/**
 * Cycles through themes and saves the preference.
 */
export function cycleTheme () {
  const settings = settingsStore.get()
  const currentTheme = settings.theme || 'system'
  const nextTheme = THEMES[THEMES.indexOf(currentTheme) + 1] || THEMES[0]
  settingsStore.update({ theme: nextTheme })
  showToast(`Set theme to "${nextTheme}"`)
}

function shouldUseDarkMode (settings) {
  // nightMode used to be true|false|null
  // Now it's 'dark'|'light'|'system'|null with null treated as 'system'
  return (settings.theme === 'dark') ||
         (prefersDarkColorScheme() && (settings.theme == null || settings.theme === 'system'))
}

function prefersDarkColorScheme () {
  return window.matchMedia('(prefers-color-scheme: dark)').matches
}

function listenToDarkMode () {
  window.matchMedia('(prefers-color-scheme: dark)').addListener(_e => {
    const settings = settingsStore.get()
    const isNight = shouldUseDarkMode(settings)
    if (settings.theme == null || settings.theme === 'system') {
      document.body.classList.toggle(DARK_MODE_CLASS, isNight)
      showToast(`Browser changed theme to "${isNight ? "dark" : "light"}"`)
    }
  })
}
