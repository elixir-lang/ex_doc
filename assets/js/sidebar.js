// Dependencies
// ------------

import $ from 'jquery'
import throttle from 'lodash.throttle'

// Constants
// ---------

const body = $('body')
const searchInput = $('#search-list')
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

function focusSearchInput () {
  searchInput.focus()
}

function setDefaultSidebarState () {
  body.removeClass(sidebarClasses)
  body.addClass(window.innerWidth > breakpoint ? sidebarOpenedClass : sidebarClosedClass)
}

// Public Methods
// --------------

export {breakpoint, closeSidebar, openSidebar, toggleSidebar, focusSearchInput}

export function initialize () {
  setDefaultSidebarState()
  let lastWindowWidth = window.innerWidth
  $(window).resize(throttle(function () {
    if (lastWindowWidth !== window.innerWidth) {
      lastWindowWidth = window.innerWidth
      setDefaultSidebarState()
    }
  }, 100))
  $('.sidebar-toggle').click(function () {
    toggleSidebar()
  })
}
