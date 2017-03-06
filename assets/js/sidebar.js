// Dependencies
// ------------

import $ from 'jquery'

// Constants
// ---------

const body = $('body')
const breakpoint = 768
const animationDuration = 300

const sidebarOpenedClass = 'sidebar-opened'
const sidebarOpeningClass = 'sidebar-opening'
const sidebarClosedClass = 'sidebar-closed'
const sidebarClosingClass = 'sidebar-closing'
const sidebarClasses = [
  sidebarOpenedClass,
  sidebarOpeningClass,
  sidebarClosedClass,
  sidebarClosingClass
].join(' ')

// Current animation state
// -----------------------

let toggling

function closeSidebar () {
  body
    .addClass(sidebarClosingClass)
    .removeClass(sidebarOpenedClass)
    .removeClass(sidebarOpeningClass)

  toggling = setTimeout(
    () => body.addClass(sidebarClosedClass).removeClass(sidebarClosingClass),
    animationDuration
  )
}

function openSidebar () {
  body
    .addClass(sidebarOpeningClass)
    .removeClass(sidebarClosedClass)
    .removeClass(sidebarClosingClass)

  toggling = setTimeout(
    () => body.addClass(sidebarOpenedClass).removeClass(sidebarOpeningClass),
    animationDuration
  )
}

function toggleSidebar () {
  const bodyClass = body.attr('class') || ''

  // Remove current animation if toggling.
  clearTimeout(toggling)

  // If body has a sidebar class invoke a correct action.
  if (bodyClass.includes(sidebarClosedClass) ||
      bodyClass.includes(sidebarClosingClass)) {
    openSidebar()
  } else {
    closeSidebar()
  }
}

function setDefaultSidebarState () {
  body.removeClass(sidebarClasses)
  body.addClass(window.innerWidth > breakpoint ? sidebarOpenedClass : sidebarClosedClass)
}

// Public Methods
// --------------

export {breakpoint, closeSidebar}

export function initialize () {
  setDefaultSidebarState()
  $(window).resize(setDefaultSidebarState)
  $('.sidebar-toggle').click(function () {
    toggleSidebar()
  })
}
