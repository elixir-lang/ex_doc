import throttle from 'lodash.throttle'
import { qs } from '../helpers'
import { SIDEBAR_CLASS_OPEN, SIDEBAR_CLASS_TRANSITION, SIDEBAR_PREF_CLOSED, SIDEBAR_PREF_OPEN, SIDEBAR_STATE_KEY, SIDEBAR_WIDTH_KEY, SMALL_SCREEN_BREAKPOINT } from './constants'
import { initialize as initializeList } from './sidebar-list'
import { isEmbedded } from '../globals'

const ANIMATION_DURATION = 300

const CONTENT_SELECTOR = '.content'
const SIDEBAR_TOGGLE_SELECTOR = '.sidebar-toggle'

const smallScreenQuery = window.matchMedia(`screen and (max-width: ${SMALL_SCREEN_BREAKPOINT}px)`)

if (!isEmbedded) {
  update()

  window.addEventListener('swup:page:view', update)

  qs(SIDEBAR_TOGGLE_SELECTOR).addEventListener('click', toggleSidebar)

  // Clicks outside small screen open sidebar should close it.
  qs(CONTENT_SELECTOR).addEventListener('click', () => {
    if (smallScreenQuery.matches && isSidebarOpen()) {
      toggleSidebar()
    }
  })

  // Update drawer on width change.
  // See https://github.com/elixir-lang/ex_doc/issues/736#issuecomment-307371291
  let lastWindowWidth = window.innerWidth
  window.addEventListener('resize', throttle(() => {
    if (lastWindowWidth === window.innerWidth) return
    lastWindowWidth = window.innerWidth
    update()
  }, 100))

  // Save sidebar width changes on user resize only.
  // Size is restored on page load in inline_html.js.
  const resizeObserver = new ResizeObserver(([entry]) => {
    if (!entry) return
    const width = entry.contentRect.width
    sessionStorage.setItem(SIDEBAR_WIDTH_KEY, width)
    document.body.style.setProperty('--sidebarWidth', `${width}px`)
  })
  // We observe on mousedown because we only care about user resize.
  const sidebar = document.getElementById('sidebar')
  sidebar.addEventListener('mousedown', () => resizeObserver.observe(sidebar))
  sidebar.addEventListener('mouseup', () => resizeObserver.unobserve(sidebar))
}

export function update () {
  const pref = sessionStorage.getItem(SIDEBAR_STATE_KEY)
  const open = pref !== SIDEBAR_PREF_CLOSED && !smallScreenQuery.matches
  if (open) initializeList()
  updateSidebar(open)
}

/**
 * Either opens or closes the sidebar depending on the current state.
 *
 * @returns {Promise} A promise resolving once the animation is finished.
 */
export function toggleSidebar () {
  const open = !isSidebarOpen()
  sessionStorage.setItem(SIDEBAR_STATE_KEY, open ? SIDEBAR_PREF_OPEN : SIDEBAR_PREF_CLOSED)
  return transitionSidebar(open)
}

function isSidebarOpen () {
  return document.body.classList.contains(SIDEBAR_CLASS_OPEN)
}

/**
- * Returns if sidebar is fully open.
- */
export function isSidebarOpened () {
  return document.body.classList.contains(SIDEBAR_CLASS_OPEN) &&
    !document.body.classList.contains(SIDEBAR_CLASS_TRANSITION)
}

function updateSidebar (open) {
  document.body.classList.toggle(SIDEBAR_CLASS_OPEN, open)
  qs(SIDEBAR_TOGGLE_SELECTOR).setAttribute('aria-expanded', open ? 'true' : 'false')
}

let transitionTimeout

function transitionSidebar (open) {
  return new Promise((resolve) => {
    document.body.classList.add(SIDEBAR_CLASS_TRANSITION)
    // Reading scrollTop forces layout so next DOM update can be transitioned.
    // eslint-disable-next-line no-unused-expressions
    document.body.scrollTop
    updateSidebar(open)
    clearTimeout(transitionTimeout)
    transitionTimeout = setTimeout(() => {
      document.body.classList.remove(SIDEBAR_CLASS_TRANSITION)
      resolve()
    }, ANIMATION_DURATION)
  })
}

/**
 * Opens the sidebar by applying an animation.
 *
 * @returns {Promise} A promise resolving once the animation is finished.
 */
export function openSidebar () {
  return transitionSidebar(true)
}
