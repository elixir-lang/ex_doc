import { settingsStore } from './settings-store'
import { showToast } from './toast'
import { DARK_MODE_CLASS, THEME_SYSTEM, THEME_DARK, THEME_LIGHT } from './constants'

const THEMES = [THEME_SYSTEM, THEME_DARK, THEME_LIGHT]

const darkMediaQuery = window.matchMedia('(prefers-color-scheme: dark)')

/**
 * Sets initial night mode state and registers to settings updates.
 */
export function initialize () {
  settingsStore.getAndSubscribe(update)
  darkMediaQuery.addEventListener('change', update)
}

function update () {
  const theme = currentTheme()
  const dark = theme === THEME_DARK || (theme !== THEME_LIGHT && darkMediaQuery.matches)
  document.body.classList.toggle(DARK_MODE_CLASS, dark)
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
  const params = new URLSearchParams(window.location.search)
  return params.get('theme') || settingsStore.get().theme || THEME_SYSTEM
}
