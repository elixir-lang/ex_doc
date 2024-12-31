// CAREFUL
// This file is inlined into each HTML document.
// Only code that must be executed ASAP belongs here.
// Imports should only bring in inlinable constants.
// Check compiled output to make sure no unnecessary code is imported.
import { SETTINGS_KEY } from '../constants'

// Immediately apply night mode preference to avoid a flash effect
try {
  const {theme} = JSON.parse(localStorage.getItem(SETTINGS_KEY) || '{}')

  if (theme === 'dark' ||
     ((theme === 'system' || theme == null) &&
       window.matchMedia('(prefers-color-scheme: dark)').matches)
  ) {
    document.body.classList.add('dark')
  }
} catch (error) { }
