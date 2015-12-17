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
  localStorage.setItem(nightMode, true)
}

function deactivateNightMode () {
  body.removeClass(nightMode)
  localStorage.removeItem(nightMode)
}

function checkForNightMode () {
  let night = localStorage.getItem(nightMode)
  if (night) {
    activateNightMode()
  }
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
