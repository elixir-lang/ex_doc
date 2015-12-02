// Dependencies
// ------------

import $ from 'jquery'

// Constants
// ---------

const body = $('body')
const sidebar = $('.sidebar')

const bodyClass = 'sidebar-closed'
const duration = 300
const displayProps = [
  '-webkit-flex',
  '-ms-flexbox',
  '-ms-flex',
  'flex'
]

function getWidth () {
  const docWidth = $(document).width()
  if (docWidth >= 1800) {
    return '425px'
  } else if (docWidth >= 1600) {
    return '375px'
  }
  return '300px'
}

let width = getWidth()

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
  width = getWidth()

  body.removeClass(bodyClass)
  displayProps.forEach(prop => {
    sidebar.css({display: prop})
  })

  sidebar.css({
    width: 0
  })
  sidebar.animate({
    '-webkit-flex-basis': width,
    '-moz-flex-basis': width,
    '-ms-flex-basis': width,
    'flex-basis': width,
    width: width
  }, duration, function () {
    sidebar.attr('style', '')
  })
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
