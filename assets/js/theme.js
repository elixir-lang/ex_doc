import { settingsStore } from './settings-store'
import { showToast } from './toast'

const DARK_MODE_CLASS = 'dark'
const THEMES = ['system', 'dark', 'light']

/**
 * Sets initial night mode state and registers to settings updates.
 */
export function initialize (theme) {
  settingsStore.getAndSubscribe(settings => {
    document.body.classList.toggle(DARK_MODE_CLASS, shouldUseDarkMode(theme || settings.theme))
  })
  listenToDarkMode()
}

/**
 * Cycles through themes and saves the preference.
 */
export function cycleTheme () {
  const nextTheme = THEMES[THEMES.indexOf(currentTheme()) + 1] || THEMES[0]
  settingsStore.update({ theme: nextTheme })
  showToast(`Set theme to "${nextTheme}"`)
}

export function currentTheme () {
  return settingsStore.get().theme || 'system'
}

function shouldUseDarkMode (theme) {
  // nightMode used to be true|false|null
  // Now it's 'dark'|'light'|'system'|null with null treated as 'system'
  return (theme === 'dark') ||
         (prefersDarkColorScheme() && (theme == null || theme === 'system'))
}

function prefersDarkColorScheme () {
  return window.matchMedia('(prefers-color-scheme: dark)').matches
}

function listenToDarkMode () {
  window.matchMedia('(prefers-color-scheme: dark)').addListener(_e => {
    const theme = settingsStore.get().theme
    const isNight = shouldUseDarkMode(theme)
    if (theme == null || theme === 'system') {
      document.body.classList.toggle(DARK_MODE_CLASS, isNight)
      showToast(`Browser changed theme to "${isNight ? 'dark' : 'light'}"`)
    }
  })
}
