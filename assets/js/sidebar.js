// Dependencies
// ------------

import $ from 'jquery'

// Constants
// ---------

const body = $('body')
const sidebar = $('.sidebar')

const bodyClass = 'sidebar-closed'
const duration = 300
const width = '300px'

function closeSidebar () {
  sidebar.animate({
    '-webkit-flex-basis': 0,
    '-moz-flex-basis': 0,
    '-ms-flex-basis': 0,
    'flex-basis': 0,
    width: 0
  }, duration, function () {
    body.addClass(bodyClass)
    sidebar.css({
      display: 'none'
    })
  })
}

function openSidebar (immediate) {
  body.removeClass(bodyClass)
  sidebar.css({
    display: 'flex',
    width: 0
  })
  sidebar.animate({
    '-webkit-flex-basis': width,
    '-moz-flex-basis': width,
    '-ms-flex-basis': width,
    'flex-basis': width,
    width: width
  }, duration)
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
}
