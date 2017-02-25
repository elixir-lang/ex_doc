// Dependencies
// ------------

import $ from 'jquery'

// Constants
// ---------

const body = $('body')
const nightMode = 'night-mode'
const nightModeToggle = $('.night-mode-toggle')

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
    if (localStorage.getItem(nightMode)) {
      activateNightMode()
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

export function initialize () {
  checkForNightMode()

  nightModeToggle.click(function () {
    toggleNightMode()
  })
}
