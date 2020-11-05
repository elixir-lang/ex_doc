import { qs } from './helpers'

const NIGHT_MODE_TOGGLE_SELECTOR = '.night-mode-toggle'
const NIGHT_MODE_CLASS = 'night-mode'
const NIGHT_MODE_KEY = 'night-mode'

/**
 * Sets initial night mode state and registers toggling.
 */
export function initialize () {
  document.body.classList.toggle(NIGHT_MODE_CLASS, shouldUseNightMode())

  qs(NIGHT_MODE_TOGGLE_SELECTOR).addEventListener('click', event => {
    toggleNightMode()
  })
}

/**
 * Toggles night mode theme and saves the preference in local storage.
 */
export function toggleNightMode () {
  const enabled = document.body.classList.contains(NIGHT_MODE_CLASS)

  if (enabled) {
    deactivateNightMode()
  } else {
    activateNightMode()
  }
}

function activateNightMode () {
  document.body.classList.add(NIGHT_MODE_CLASS)
  setNightModePreference(true)
}

function deactivateNightMode () {
  document.body.classList.remove(NIGHT_MODE_CLASS)
  setNightModePreference(false)
}

function shouldUseNightMode () {
  const nightModePreference = getNightModePreference()

  return (nightModePreference === true) ||
    (nightModePreference === null && prefersDarkColorScheme())
}

function prefersDarkColorScheme () {
  return window.matchMedia('(prefers-color-scheme: dark)').matches
}

function getNightModePreference () {
  try {
    return JSON.parse(localStorage.getItem(NIGHT_MODE_KEY))
  } catch (error) {
    return null
  }
}

function setNightModePreference (enabled) {
  try {
    localStorage.setItem(NIGHT_MODE_KEY, JSON.stringify(enabled))
  } catch (error) { }
}
