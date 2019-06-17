// Dependencies
// ------------

import $ from 'jquery'
import tooltipBodyTemplate from '../templates/tooltip-body.handlebars'
import tooltipLayoutTemplate from '../templates/tooltip-layout.handlebars'

// Constants
// ---------
const footerSelector = 'footer' // `Enable/Disable tooltips` button will be displayed in the footer
const tooltipActivators = '.content a code, .detail-header .specs a' // Elements that can activate the tooltip
const tooltipSelector = '#tooltip' // Tooltip root
const tooltipIframeSelector = '#tooltip .tooltip-iframe' // Iframe inisde the tooltip, will be used to load external pages
const contentInner = 'body .content-inner' // Element containing the documentation text
const spacingBase = 10 // Used as the min. distance from window edges and links
const minBottomSpacing = spacingBase * 5 // Min. space needed between the bottom of the bottom of the page and the bottom edge of the tooltip
const minWindowSize = { // Tooltips won't be displayed if width/height of the viewport is smaller than this
  height: 450,
  width: 768
}
// Tooltip will appear only if the mouse cursor stays on the link for at least 150ms.
// This way tooltips will not appear if we are scrooling the page or just moving the cursor around.
const hoverDelayTime = 150
const typesPage = 'typespecs.html' // Page containing info about basic type, built-in types and literals.
const tooltipsToggleSelector = '.tooltips-toggle' // `Enable/Disable tooltips` button
const tooltipsDisabledStorageKey = 'tooltipsDisabled' // Local Storage key Used to store tooltips settings

let tooltipElement = null // Will store the jQuery selector for the tooltip root
let currentLinkElement = null // Element that the cursor is hovering over
let currentRequestId = null // ID of the request we're waiting for
let showTimeoutAnimation = null // Timeout ID related to the tooltip show animation
let hideTimeoutVisibility = null // Timeout ID related to the tooltip hide animation
let hoverDelayTimeout = null // Timeout ID related to the `hoverDelayTime` described above

// Switches tooltips OFF and stores the choice in localStorage.
function deactivateTooltips () {
  try { localStorage.setItem(tooltipsDisabledStorageKey, true) } catch (e) { }
  updateToggleLink()
}

// Switches tooltips ON and stores the choice in localStorage.
function activateTooltips () {
  try { localStorage.removeItem(tooltipsDisabledStorageKey) } catch (e) { }
  updateToggleLink()
}

/**
 * Checks if tooltips are disabled.
 *
 * @returns {boolean} `true` if tooltips are disabled, `false` otherwise.
 */
function areTooltipsDisabled () {
  try {
    return !!localStorage.getItem(tooltipsDisabledStorageKey)
  } catch (e) { }

  return false
}

// If tooltips are disabled switches them on. If they are enabled switches them on.
function toggleTooltipsDisabled () {
  areTooltipsDisabled() ? activateTooltips() : deactivateTooltips()
}

/**
 * Updates text of the link used to disable/enable tooltips.
 *
 * If tooltips are disabled `Enable tooltips` text is displayed.
 * If tooltips are enabled `Disable tooltips` text is displayed.
 */
function updateToggleLink () {
  $(tooltipsToggleSelector).attr('data-is-disabled', areTooltipsDisabled().toString())
}

/**
 * Check how much free space there is areound the tooltip.
 *
 * @param {Object} event `message` event data
 */

function receivePopupMessage (event) {
  if (event.data.requestId !== currentRequestId) { return }
  if (event.data.ready !== true) { return }

  showTooltip(event.data.hint)
}

// Triggered when the mouse cursor is over a link that supports the tooltip.
function hoverStart () {
  if (areTooltipsDisabled()) { return }
  if (window.innerWidth < minWindowSize.width || window.innerHeight < minWindowSize.height) {
    return
  }

  currentLinkElement = $(this)
  if (currentLinkElement.prop('tagName') !== 'A') {
    currentLinkElement = $(this).parent()
  }

  currentRequestId = uid()

  hoverDelayTimeout = setTimeout(function () {
    hideTimeoutVisibility && clearTimeout(hideTimeoutVisibility)

    tooltipElement.removeClass('tooltip-visible')
    tooltipElement.removeClass('tooltip-shown')

    prepareTooltips()
  }, hoverDelayTime)
}

// Triggered when the mouse cursor leaves the tooltip-enabled link
function hoverEnd () {
  if (areTooltipsDisabled()) { return }

  showTimeoutAnimation && clearTimeout(showTimeoutAnimation)
  hoverDelayTimeout && clearTimeout(hoverDelayTimeout)

  currentLinkElement = null
  hideTooltip()
}

// Checks position and scroll of content and link elements and chooses the best position for the tooltip.
function updateTooltipPosition () {
  if (!currentLinkElement) { return }

  const tooltipElement = $(tooltipSelector)

  const tooltipActivatorBoundingRect = currentLinkElement[0].getBoundingClientRect()
  const contentInnerBoundingRect = $(contentInner)[0].getBoundingClientRect()

  const tooltipWidth = measureTooltipWidth(tooltipElement)
  const relativeBoundingRect = getRelativeBoudningRect(tooltipActivatorBoundingRect, contentInnerBoundingRect)
  const space = calculateSpaceAroundLink(relativeBoundingRect, tooltipActivatorBoundingRect, contentInnerBoundingRect)

  if (space.left + tooltipWidth + spacingBase < window.innerWidth) {
    tooltipElement.css('left', relativeBoundingRect.left)
    tooltipElement.css('right', 'auto')
  } else {
    // Tooltip looks better if there is some space between it and the left menu.
    let left = relativeBoundingRect.right - tooltipWidth
    if (left < spacingBase) {
      left = spacingBase
    }
    tooltipElement.css('left', left)
    tooltipElement.css('right', 'auto')
  }

  const tooltipHeight = measureTooltipHeight(tooltipElement)

  if (space.bottom > tooltipHeight + minBottomSpacing) {
    tooltipElement.css('top', relativeBoundingRect.bottom + spacingBase)
  } else {
    tooltipElement.css('top', relativeBoundingRect.top - tooltipHeight - spacingBase)
  }
}

/**
 * Since the tooltip is displayed inside the contentInner (this way it can easily inherit all the basic styles),
 * we calculate it's relative coordinates to position it correctly.
 *
 * @param {DOMRect} linkRect dimensions and position of the link that triggered the tooltip
 * @param {DOMRect} contentRect dimensions and position of the contentInner
 *
 * @returns {DOMRect} dimensions and position of the link element relative to the contentInner
 */
function getRelativeBoudningRect (linkRect, contentRect) {
  return {
    top: linkRect.top - contentRect.top,
    bottom: linkRect.bottom - contentRect.top,
    left: linkRect.left - contentRect.left,
    right: linkRect.right - contentRect.left,
    x: linkRect.x - contentRect.x,
    y: linkRect.y - contentRect.y,
    width: linkRect.width,
    height: linkRect.height
  }
}

/**
 * Check how much free space there is around the tooltip.
 * we calculate it's relative coordinates to position it correctly.
 *
 * @param {DOMRect} linkRect dimensions and position of the link that triggered the tooltip
 * @param {DOMRect} contentRect dimensions and position of the contentInner
 * @param {DOMRect} relativeRect dimensions and position of the link relative to the contentInner
 *
 * @returns {Object} free space on the top/right/bottom/left of the link that triggered the tooltip
 */
function calculateSpaceAroundLink (relativeRect, linkRect, contentRect) {
  return {
    left: linkRect.x,
    right: contentRect.width - linkRect.x + linkRect.width,
    top: relativeRect.y - window.scrollY,
    bottom: window.innerHeight - (relativeRect.y - window.scrollY) + relativeRect.height
  }
}

// Prepares the tooltip DOM (without showing it).
function prepareTooltips () {
  updateTooltipPosition()

  if (!currentLinkElement) { return }

  let href = currentLinkElement.attr('href')

  if (!href) { return }

  if (href.charAt(0) === '#') {
    href = `${window.location.pathname}${href}`
  }

  const focusedHref = rewriteHref(href)
  $(tooltipIframeSelector).attr('src', focusedHref)
}

// Shows tooltip and starts it's animation.
function showTooltip (hint) {
  const html = tooltipBodyTemplate({
    isModule: hint.kind === 'module',
    isType: hint.kind === 'type',
    isBuiltInType: hint.typeCategory === 'builtInType',
    hint: hint
  })

  tooltipElement.find('.tooltip-body').html(html)

  tooltipElement.addClass('tooltip-visible')

  updateTooltipPosition()
  showTimeoutAnimation = setTimeout(() => {
    tooltipElement.addClass('tooltip-shown')
  }, 10)
}

// Hides the tooltip
function hideTooltip () {
  tooltipElement.removeClass('tooltip-shown')
  hideTimeoutVisibility = setTimeout(() => {
    tooltipElement.removeClass('tooltip-visible')
  }, 300)
}

/**
 * Modifies the link, adding parameters needed to trigger hints extraction.
 *
 * @param {string} href link to the page
 *
 * @returns {string} link with parameters added
 */
function rewriteHref (href) {
  let typeInfo = ''

  if (isTypesPageLink(href)) {
    const typeName = encodeURIComponent(currentLinkElement.text())
    typeInfo = `&typeName=${typeName}`
  }

  return href.replace('.html', `.html?hint=true&requestId=${currentRequestId}${typeInfo}`)
}

/**
 * Is the current link poinitng to the typespecs page?
 *
 * @param {string} href link to the page
 *
 * @returns {boolean} `true` if link points to the typespecs page, `false` otherwise
 */
function isTypesPageLink (href) {
  return (href.indexOf(typesPage) === 0 || href.indexOf(`/${typesPage}`) >= 0)
}

/**
 * Generates an id that will be included, as a param, in the iFrame URL.
 * Message that comes back from the iFrame will send back this id - this will help to avoid reace conditions.
 *
 * @returns {string} unique ID
 */
function uid () {
  return Math.random().toString(36).substr(2, 9)
}

/**
 * Measures height of the tooltips. Used when positioning the tooltip vertically.
 *
 * @param {object} tooltipElement jQuery element targeting the tooltip
 *
 * @returns {number} height of the tooltip
 */
function measureTooltipHeight (tooltipElement) {
  return tooltipElement[0].getBoundingClientRect().height
}

/**
 * Measures width of the tooltips. Used when positioning the tooltip horizontally.
 *
 * @param {object} tooltipElement jQuery element targeting the tooltip
 *
 * @returns {number} width of the tooltip
 */
function measureTooltipWidth (tooltipElement) {
  return tooltipElement[0].getBoundingClientRect().width
}

// Public Methods
// --------------

export function initialize () {
  window.addEventListener('message', receivePopupMessage, false)

  $(contentInner).append(tooltipLayoutTemplate())
  tooltipElement = $(tooltipSelector)

  $(tooltipActivators).hover(hoverStart, hoverEnd)

  $(footerSelector).on('click', tooltipsToggleSelector, function () {
    toggleTooltipsDisabled()
  })

  updateToggleLink()
}
