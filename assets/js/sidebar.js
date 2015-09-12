// Dependencies
// ------------

import $ from 'jquery'

// Constants
// ---------

const body = $('body')
const sidebar = $('.sidebar')

const bodyClass = 'sidebar-open'
const duration = 300
const width = '300px'

function closeSidebar () {
  sidebar.animate({
    'flex-basis': 0
  }, duration, function () {
    body.removeClass(bodyClass)
  })
}

function openSidebar (immediate) {
  var dur = duration
  if (immediate) {
    dur = 0
  }

  body.addClass(bodyClass)
  sidebar.animate({
    '-webkit-flex-basis': width,
    '-moz-flex-basis': width,
    '-ms-flex-basis': width,
    'flex-basis': width
  }, dur)
}

function toggleSidebar () {
  if (sidebar.css('display') !== 'none') {
    closeSidebar()
  } else {
    openSidebar()
  }
}

// Public Methods
// --------------

export function initialize () {
  $('.sidebar-toggle').click(function () {
    toggleSidebar()
  })

  if ($(window).width() > 768) {
    openSidebar(true)
  }
}
