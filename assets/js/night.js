// Dependencies
// ------------

import {qs} from './helpers'

// Constants
// ---------

const body = qs('body')
const nightMode = 'night-mode'
const nightModeToggleSelector = '.night-mode-toggle'

function activateNightMode () {
  body.classList.add(nightMode)
  try { localStorage.setItem(nightMode, true) } catch (e) { }
}

function deactivateNightMode () {
  body.classList.remove(nightMode)
  try { localStorage.removeItem(nightMode) } catch (e) { }
}

function checkForNightMode () {
  try {
    const userWantsNightMode = localStorage.getItem(nightMode)

    if (userWantsNightMode != null) {
      if (userWantsNightMode === true) {
        activateNightMode()
      }
    } else if (matchMedia('(prefers-color-scheme: dark)').matches) {
      body.classList.add(nightMode)
    }
  } catch (e) { }
}

function toggleNightMode () {
  if (body.classList.contains(nightMode)) {
    deactivateNightMode()
  } else {
    activateNightMode()
  }
}

// Public Methods
// --------------

export {toggleNightMode}

export function initialize () {
  checkForNightMode()

  qs(nightModeToggleSelector).addEventListener('click', function () {
    toggleNightMode()
  })
}
