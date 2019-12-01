// Dependencies
// ------------

import $ from 'jquery'

// Constants
// ---------

const body = $('body')
const nightMode = 'night-mode'
const nightModeToggleSelector = '.night-mode-toggle'

function activateNightMode () {
  body.addClass(nightMode)
  try { localStorage.setItem(nightMode, true) } catch (e) { }
}

function deactivateNightMode () {
  body.removeClass(nightMode)
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
      body.addClass(nightMode)
    }
  } catch (e) { }
}

function toggleNightMode () {
  if (body.hasClass(nightMode)) {
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

  body.on('click', nightModeToggleSelector, function () {
    toggleNightMode()
  })
}
