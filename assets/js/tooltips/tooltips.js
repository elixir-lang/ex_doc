import { qs, qsAll } from '../helpers'
import { settingsStore } from '../settings-store'
import { cancelHintFetchingIfAny, getHint, HINT_KIND, isValidHintHref } from './hints'
import tooltipBodyTemplate from '../handlebars/templates/tooltip-body.handlebars'

const TOOLTIP_HTML = '<div class="tooltip"><div class="tooltip-body"></div></div>'

// Elements that can activate the tooltip.
const TOOLTIP_ACTIVATORS_SELECTOR = '.content a:not([data-no-tooltip])'
// Tooltip root element.
const TOOLTIP_SELECTOR = '.tooltip'
// Tooltip content element.
const TOOLTIP_BODY_SELECTOR = '.tooltip .tooltip-body'
// Element containing the documentation text.
const CONTENT_INNER_SELECTOR = 'body .content-inner'

// Hash included in links pointing to the top
const CONTENT_HASH = '#content'

const TOOLTIP_SHOWN_CLASS = 'tooltip-shown'
// The minimum distance from window edges and links.
const SPACING_BASE = 10
// The minimum space needed between the bottom of the page and the bottom edge of the tooltip.
const MIN_BOTTOM_SPACING = SPACING_BASE * 4
// The minimum required viewport size to show tooltips.
const MIN_WIDTH = 768
const MIN_HEIGHT = 450
// The minimum time mouse cursor must stay on link for the tooltip to be loaded.
// This prevents from triggering tooltips by accident while scrolling and moving the cursor around.
const HOVER_DELAY_MS = 100

const state = {
  // Element that the cursor is hovering over
  currentLinkElement: null,
  // The timeout used for delaying the hover action.
  hoverDelayTimeout: null
}

/**
 * Initializes tooltips handling.
 */
export function initialize () {
  qsAll(TOOLTIP_ACTIVATORS_SELECTOR).forEach(element => {
    if (!linkElementEligibleForTooltip(element)) { return }

    element.addEventListener('mouseenter', handleHoverStart)
    element.addEventListener('mouseleave', handleHoverEnd)
  })
}

/**
 * Decides whether we may attempt to load a tooltip for the given
 * link element, or if it should just be ignored up-front.
 */
function linkElementEligibleForTooltip (linkElement) {
  // Skip link to the module page we are already on.
  if (isHrefToSelf(linkElement.href)) { return false }

  // Skip unsupported URLs right away.
  if (!isValidHintHref(linkElement.href)) { return false }

  return true
}

/**
 * Checks if the given link points to the module page we are currently on.
 */
function isHrefToSelf (href) {
  const targetPage = href.replace(CONTENT_HASH, '')
  const currentPage = window.location.href.split('#')[0]

  return currentPage === targetPage
}

/** @param {MouseEvent} event */
function handleHoverStart (event) {
  if (
    window.innerWidth < MIN_WIDTH ||
    window.innerHeight < MIN_HEIGHT ||
    !settingsStore.get().tooltips
  ) { return }

  const element = event.currentTarget

  state.currentLinkElement = element

  state.hoverDelayTimeout = setTimeout(() => {
    getHint(element.href)
      .then(renderTooltip)
      .catch(() => {})
  }, HOVER_DELAY_MS)
}

function renderTooltip (hint) {
  const tooltipBodyHtml = tooltipBodyTemplate({
    isPlain: hint.kind === HINT_KIND.plain,
    hint
  })

  let tooltipBody = qs(TOOLTIP_BODY_SELECTOR)
  if (!tooltipBody) {
    qs(CONTENT_INNER_SELECTOR).insertAdjacentHTML('beforeend', TOOLTIP_HTML)
    tooltipBody = qs(TOOLTIP_BODY_SELECTOR)
  }
  tooltipBody.innerHTML = tooltipBodyHtml

  updateTooltipPosition()

  // Animate tooltip in.
  qs(TOOLTIP_SELECTOR).classList.add(TOOLTIP_SHOWN_CLASS)
}

function handleHoverEnd () {
  if (!state.currentLinkElement) { return }

  clearTimeout(state.hoverDelayTimeout)
  cancelHintFetchingIfAny()
  state.currentLinkElement = null
  qs(TOOLTIP_SELECTOR)?.classList.remove(TOOLTIP_SHOWN_CLASS)
}

/**
 * Updates the tooltip position to the best placement next to the hovered link.
 */
function updateTooltipPosition () {
  if (!state.currentLinkElement) { return }

  const tooltipElement = qs(TOOLTIP_SELECTOR)

  const linkBoundingRect = state.currentLinkElement.getBoundingClientRect()
  const contentInnerBoundingRect = qs(CONTENT_INNER_SELECTOR).getBoundingClientRect()
  const tooltipBoundingRect = tooltipElement.getBoundingClientRect()
  const relativeBoundingRect = getRelativeBoundingRect(linkBoundingRect, contentInnerBoundingRect)

  // Since the tooltip is displayed inside `contentInner` (this way it can easily inherit all the basic styles),
  // we calculate it's coordinates relatively to this `contentInner`.

  if (linkBoundingRect.left + tooltipBoundingRect.width + SPACING_BASE < window.innerWidth) {
    tooltipElement.style.left = `${relativeBoundingRect.left}px`
    tooltipElement.style.right = 'auto'
  } else {
    // Tooltip looks better if there is some space between it and the left menu.
    const left = Math.max(relativeBoundingRect.right - tooltipBoundingRect.width, SPACING_BASE)
    tooltipElement.style.left = `${left}px`
    tooltipElement.style.right = 'auto'
  }

  if (linkBoundingRect.bottom + tooltipBoundingRect.height + MIN_BOTTOM_SPACING < window.innerHeight) {
    tooltipElement.style.top = `${relativeBoundingRect.bottom + SPACING_BASE}px`
  } else {
    tooltipElement.style.top = `${relativeBoundingRect.top - tooltipBoundingRect.height - SPACING_BASE}px`
  }
}

/**
 * Calculates position of an element with respect to the given container element,
 * so that all the returned properties are relative.
 *
 * @param {DOMRect} elementRect Dimensions and position of an element.
 * @param {DOMRect} containerRect Dimensions and position of a container used as the reference for positioning.
 * @returns {DOMRect} Dimensions and position of the given element relative to the container.
 */
function getRelativeBoundingRect (elementRect, containerRect) {
  return {
    top: elementRect.top - containerRect.top,
    bottom: elementRect.bottom - containerRect.top,
    left: elementRect.left - containerRect.left,
    right: elementRect.right - containerRect.left,
    x: elementRect.x - containerRect.x,
    y: elementRect.y - containerRect.y,
    width: elementRect.width,
    height: elementRect.height
  }
}
