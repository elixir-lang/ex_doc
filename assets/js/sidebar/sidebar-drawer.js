import throttle from 'lodash.throttle'
import { qs } from '../helpers'

const BREAKPOINT = 768
const ANIMATION_DURATION = 300

const SIDEBAR_TOGGLE_SELECTOR = '.sidebar-toggle'
const CONTENT_SELECTOR = '.content'

const userPref = {
  CLOSED: 'closed',
  OPEN: 'open',
  NO_PREF: 'no_pref'
}

const SIDEBAR_CLASS = {
  opened: 'sidebar-opened',
  opening: 'sidebar-opening',
  closed: 'sidebar-closed',
  closing: 'sidebar-closing'
}

const SIDEBAR_CLASSES = Object.values(SIDEBAR_CLASS)

const state = {
  // Keep track of the current timeout to clear it if needed
  togglingTimeout: null,
  // Record window width on resize to update sidebar state only when it actually changes
  lastWindowWidth: window.innerWidth,
  // No_PREF is defaults to OPEN behavior
  sidebarPreference: userPref.NO_PREF
}

/**
 * Initializes the toggleable sidebar drawer.
 */
export function initialize () {
  setDefaultSidebarState()
  addEventListeners()
}

function setDefaultSidebarState () {
  // check & set persistent session state
  const persistent_session_state = sessionStorage.getItem('sidebar_state')
  if (persistent_session_state == 'opened') return setClass(SIDEBAR_CLASS.opened)
  if (persistent_session_state == 'closed') return setClass(SIDEBAR_CLASS.closed)

  // else
  setClass(isScreenSmall() ? SIDEBAR_CLASS.closed : SIDEBAR_CLASS.opened)
}

function isScreenSmall () {
  return window.matchMedia(`screen and (max-width: ${BREAKPOINT}px)`).matches
}

function setClass (...classes) {
  document.body.classList.remove(...SIDEBAR_CLASSES)
  document.body.classList.add(...classes)
}

function addEventListeners () {
  qs(SIDEBAR_TOGGLE_SELECTOR).addEventListener('click', (event) => {
    toggleSidebar()
    setPreference()
  })

  qs(CONTENT_SELECTOR).addEventListener('click', (event) => {
    closeSidebarIfSmallScreen()
  })

  window.addEventListener(
    'resize',
    throttle((event) => {
      adoptSidebarToWindowSize()
    }, 100)
  )
}

/**
 * Either opens or closes the sidebar depending on the current state.
 *
 * @returns {Promise} A promise resolving once the animation is finished.
 */
export function toggleSidebar () {
  if (isSidebarOpen()) {
    return closeSidebar()
  } else {
    return openSidebar()
  }
}

function isSidebarOpen () {
  return (
    document.body.classList.contains(SIDEBAR_CLASS.opened) ||
    document.body.classList.contains(SIDEBAR_CLASS.opening)
  )
}

/**
 * Opens the sidebar by applying an animation.
 *
 * @returns {Promise} A promise resolving once the animation is finished.
 */
export function openSidebar () {
  clearTimeoutIfAny()
  setClass(SIDEBAR_CLASS.opening)
  sessionStorage.setItem('sidebar_state', 'opened')

  return new Promise((resolve, reject) => {
    state.togglingTimeout = setTimeout(() => {
      setClass(SIDEBAR_CLASS.opened)
      resolve()
    }, ANIMATION_DURATION)
  })
}

/**
 * Closes the sidebar by applying an animation.
 *
 * @returns {Promise} A promise resolving once the animation is finished.
 */
export function closeSidebar () {
  clearTimeoutIfAny()
  setClass(SIDEBAR_CLASS.closing)
  sessionStorage.setItem('sidebar_state', 'closed')

  return new Promise((resolve, reject) => {
    state.togglingTimeout = setTimeout(() => {
      setClass(SIDEBAR_CLASS.closed)
      resolve()
    }, ANIMATION_DURATION)
  })
}

function clearTimeoutIfAny () {
  if (state.togglingTimeout) {
    clearTimeout(state.togglingTimeout)
    state.togglingTimeout = null
  }
}

/**
 * Handles updating the sidebar state on window resize
 *
 * WHEN the window width has changed
 * AND the user sidebar preference is OPEN or NO_PREF
 * THEN adjust the sidebar state according to screen size
 */
function adoptSidebarToWindowSize () {
  // See https://github.com/elixir-lang/ex_doc/issues/736#issuecomment-307371291
  if (state.lastWindowWidth !== window.innerWidth) {
    state.lastWindowWidth = window.innerWidth
    if (
      state.sidebarPreference === userPref.OPEN ||
      state.sidebarPreference === userPref.NO_PREF
    ) {
      setDefaultSidebarState()
    }
  }
}

function closeSidebarIfSmallScreen () {
  const sidebarCoversContent = isScreenSmall()
  if (sidebarCoversContent && isSidebarOpen()) {
    closeSidebar()
  }
}

/**
 * Track the sidebar preference for the user
 */
function setPreference () {
  switch (state.sidebarPreference) {
    case userPref.OPEN:
      state.sidebarPreference = userPref.CLOSED
      break
    case userPref.CLOSED:
      state.sidebarPreference = userPref.OPEN
      break
    case userPref.NO_PREF:
      isSidebarOpen()
        ? (state.sidebarPreference = userPref.OPEN)
        : (state.sidebarPreference = userPref.CLOSED)
  }
}
