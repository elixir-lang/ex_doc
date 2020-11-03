// Dependencies
// ------------

import throttle from 'lodash.throttle'
import {qs} from './helpers'

// Constants
// ---------

const body = qs('body')
const searchInput = qs('#search-list')
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
]

// Current animation state
// -----------------------

let toggling

function closeSidebar () {
  body.classList.add(sidebarClosingClass)
  body.classList.remove(sidebarOpenedClass, sidebarOpeningClass)

  toggling = setTimeout(() => {
    body.classList.add(sidebarClosedClass)
    body.classList.remove(sidebarClosingClass)
  }, animationDuration)
}

function openSidebar () {
  body.classList.add(sidebarOpeningClass)
  body.classList.remove(sidebarClosedClass, sidebarClosingClass)

  toggling = setTimeout(() => {
    body.classList.add(sidebarOpenedClass)
    body.classList.remove(sidebarOpeningClass)
  }, animationDuration)
}

function toggleSidebar () {
  // Remove current animation if toggling.
  clearTimeout(toggling)

  // If body has a sidebar class invoke a correct action.
  if (body.classList.contains(sidebarClosedClass) ||
      body.classList.contains(sidebarClosingClass)) {
    openSidebar()
  } else {
    closeSidebar()
  }
}

function focusSearchInput () {
  searchInput.focus()
}

function isScreenSmall () {
  return window.innerWidth <= breakpoint
}

function setDefaultSidebarState () {
  body.classList.remove(...sidebarClasses)
  body.classList.add(isScreenSmall() ? sidebarClosedClass : sidebarOpenedClass)
}

// Public Methods
// --------------

export {breakpoint, closeSidebar, openSidebar, toggleSidebar, focusSearchInput}

export function initialize () {
  setDefaultSidebarState()

  let lastWindowWidth = window.innerWidth
  window.addEventListener('resize', throttle(function () {
    if (lastWindowWidth !== window.innerWidth) {
      lastWindowWidth = window.innerWidth
      setDefaultSidebarState()
    }
  }, 100))

  qs('.sidebar-toggle').addEventListener('click', toggleSidebar)

  qs('.content').addEventListener('click', (event) => {
    const sidebarCoversContent = isScreenSmall()
    if (sidebarCoversContent) { closeSidebar() }
  })
}
