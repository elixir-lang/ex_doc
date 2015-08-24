var $ = require('jquery')

var body = $('body')
var sidebar = $('.sidebar')

var bodyClass = 'sidebar-open'
var duration = 300
var width = '300px'

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
  sidebar.animate({
    '-webkit-flex-basis': width,
    '-moz-flex-basis': width,
    '-ms-flex-basis': width,
    'flex-basis': width
  }, dur, function () {
    body.addClass(bodyClass)
  })
}

function toggleSidebar () {
  if (sidebar.width() > 0) {
    closeSidebar()
  } else {
    openSidebar()
  }
}

function init () {
  $('.sidebar-toggle').click(function () {
    toggleSidebar()
  })

  if ($(window).width() > 768) {
    openSidebar(true)
  }
}

module.exports = {
  init: init
}
