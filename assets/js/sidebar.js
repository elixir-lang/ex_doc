// Dependencies
// ------------

import $ from 'jquery'

// Constants
// ---------

const body = $('body')
const breakpoint = 768
const sidebarOpenedClass = 'sidebar-opened'
const sidebarClosedClass = 'sidebar-closed'

function closeSidebar () {
  body.addClass(sidebarClosedClass).removeClass(sidebarOpenedClass)
}

function openSidebar () {
  body.addClass(sidebarOpenedClass).removeClass(sidebarClosedClass)
}

function toggleSidebar () {
  const bodyClass = body.attr('class')
  // If body has a sidebar class invoke a correct action.
  if (bodyClass) {
    bodyClass === sidebarClosedClass ? openSidebar() : closeSidebar()
  // Otherwise check the width of window to know which action to invoke.
  } else {
    window.screen.width > breakpoint ? closeSidebar() : openSidebar()
  }
}

// Public Methods
// --------------

export function initialize () {
  $('.sidebar-toggle').click(function () {
    toggleSidebar()
  })
}
