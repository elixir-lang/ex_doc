// CAREFUL
// This file is inlined into each HTML document.
// Only code that must be executed ASAP belongs here.
// Imports should only bring in inlinable constants.
// Check compiled output to make sure no unnecessary code is imported.
import { DARK_MODE_CLASS, SETTINGS_KEY, THEME_DARK, THEME_LIGHT } from '../constants'
import { SIDEBAR_CLASS_OPEN, SIDEBAR_PREF_CLOSED, SIDEBAR_STATE_KEY, SIDEBAR_WIDTH_KEY, SMALL_SCREEN_BREAKPOINT } from '../sidebar/constants'

const params = new URLSearchParams(window.location.search)

// Immediately apply night mode preference to avoid a flash effect.
// Should match logic in theme.js.
const theme = params.get('theme') || JSON.parse(localStorage.getItem(SETTINGS_KEY) || '{}').theme
if (theme === THEME_DARK ||
     (theme !== THEME_LIGHT &&
       window.matchMedia('(prefers-color-scheme: dark)').matches)
) {
  document.body.classList.add(DARK_MODE_CLASS)
}

// Set sidebar state and width.
// Should match logic in sidebar-drawer.js.
const sidebarPref = sessionStorage.getItem(SIDEBAR_STATE_KEY)
const open = sidebarPref !== SIDEBAR_PREF_CLOSED && !window.matchMedia(`screen and (max-width: ${SMALL_SCREEN_BREAKPOINT}px)`).matches
document.body.classList.toggle(SIDEBAR_CLASS_OPEN, open)

const sidebarWidth = sessionStorage.getItem(SIDEBAR_WIDTH_KEY)
if (sidebarWidth) {
  document.body.style.setProperty('--sidebarWidth', `${sidebarWidth}px`)
}
