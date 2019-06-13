// Dependencies
// ------------

import $ from 'jquery'
import popoverTemplate from './templates/popover.handlebars'

// Constants
// ---------
const popoverable = '.content a code, .signature .specs a'
const popoverSelector = '#popover'
const popoverIframeSelector = '#popover .popover-iframe'
const contentInner = 'body .content-inner'
const spacingBase = 10
const minBottomSpacing = spacingBase * 5
const hoverDelayTime = 150
const typesPage = 'typespecs.html'
let popoverElement = null
let currentLinkElement = null
let currentRequestId = null
let showTimeoutVisibility = null
let showTimeoutAnimation = null
let hideTimeoutVisibility = null
let hoverDelayTimeout = null

function updatePopoverPosition () {
  if (!currentLinkElement) { return }

  const popoverElement = $(popoverSelector)

  const popoverableBoundingRect = currentLinkElement[0].getBoundingClientRect()
  const contentInnerBoundingRect = $(contentInner)[0].getBoundingClientRect()
  const popoverBoundingRect = popoverElement[0].getBoundingClientRect()

  const popoverHeight = popoverBoundingRect.height
  const popoverWidth = popoverBoundingRect.height

  // Since the popover is displayed inside the contentInner (this way it can easily inherit all the basic styles),
  // we will need to know it's relative coordinates to position it correctly.
  const relativeBoundingRect = {
    top: popoverableBoundingRect.top - contentInnerBoundingRect.top,
    bottom: popoverableBoundingRect.bottom - contentInnerBoundingRect.top,
    left: popoverableBoundingRect.left - contentInnerBoundingRect.left,
    right: popoverableBoundingRect.right - contentInnerBoundingRect.left,
    x: popoverableBoundingRect.x - contentInnerBoundingRect.x,
    y: popoverableBoundingRect.y - contentInnerBoundingRect.y,
    width: popoverableBoundingRect.width,
    height: popoverableBoundingRect.height
  }

  let space = {
    left: popoverableBoundingRect.x,
    right: contentInnerBoundingRect.width - popoverableBoundingRect.x + popoverableBoundingRect.width,
    top: relativeBoundingRect.y - window.scrollY,
    bottom: window.innerHeight - (relativeBoundingRect.y - window.scrollY) + relativeBoundingRect.height
  }

  console.log('popoverableBoudingRect', popoverableBoundingRect)
  console.log('relativeBoundingRect', relativeBoundingRect)
  console.log('contentInnerBoundingRect', contentInnerBoundingRect)

  if (space.bottom > popoverHeight + minBottomSpacing) {
    popoverElement.css('top', relativeBoundingRect.bottom + spacingBase)
  } else {
    popoverElement.css('top', relativeBoundingRect.top - popoverHeight - spacingBase)
  }

  if (space.left + popoverWidth < window.innerWidth) {
    popoverElement.css('left', relativeBoundingRect.left)
    popoverElement.css('right', 'auto')
  } else {
    // Popover looks better if there is some space between it and the menu.
    let left = relativeBoundingRect.right - popoverWidth
    if (left < spacingBase) {
      left = spacingBase
    }
    popoverElement.css('left', left)
    popoverElement.css('right', 'auto')
  }
}

// Prepares popover without showing it.
function preparePopover () {
  updatePopoverPosition()

  if (!currentLinkElement) { return }

  let href = currentLinkElement.attr('href')

  if (!href) { return }

  if (href.charAt(0) === '#') {
    href = `${window.location.pathname}${href}`
  }

  const focusedHref = rewriteHref(href)
  $(popoverIframeSelector).attr('src', focusedHref)
}

// Show popover and start it's animation.
function showPopover (summary) {
  const html = popoverTemplate({
    isModule: summary.type === 'page',
    isType: summary.type === 'type',
    isBuiltInType: summary.typeCategory === 'builtInType',
    summary: summary
  })

  popoverElement.find('.popover-body').html(html)

  popoverElement.addClass('popover-visible')

  updatePopoverPosition()
  showTimeoutAnimation = setTimeout(() => {
    popoverElement.addClass('popover-shown')
  }, 10)
}

function hidePopover () {
  popoverElement.removeClass('popover-shown')
  hideTimeoutVisibility = setTimeout(() => {
    popoverElement.removeClass('popover-visible')
  }, 300)
}

function receivePopupMessage (event) {
  console.log('receivePopupMessage', event)
  if (event.data.requestId !== currentRequestId) { return }
  if (event.data.ready !== true) { return }

  showPopover(event.data.summary)
}

function rewriteHref (href) {
  let typeInfo = ''

  if (isTypesPageLink(href)) {
    console.log('is type page - adding link')
    typeInfo = `&typeName=${currentLinkElement.text()}`
  } else {
    console.log('not a type page')
  }

  return href.replace('.html', `.html?focused=true&requestId=${currentRequestId}${typeInfo}`)
}

function isTypesPageLink (href) {
  console.log("typesPage href", href, typesPage)
  return (href.indexOf(typesPage) === 0 || href.indexOf(`/${typesPage}`) >= 0)
}

function uid () {
  return Math.random().toString(36).substr(2, 9)
}

// Public Methods
// --------------

export function initialize () {
  window.addEventListener('message', receivePopupMessage, false)

  $(contentInner).append('<div id="popover"><div class="popover-body"></div><iframe class="popover-iframe"></iframe></div>')
  popoverElement = $(popoverSelector)

  $(popoverable).hover(function () {
    if (window.innerWidth < 768 || window.innerHeight < 400) {
      return
    }

    currentLinkElement = $(this)
    console.log("tagname", currentLinkElement.prop('tagName'))
    if (currentLinkElement.prop('tagName') !== 'A') {
      currentLinkElement = $(this).parent()
    } else {
      console.log("loading type")
    }

    currentRequestId = uid()

    hoverDelayTimeout = setTimeout(function () {
      hideTimeoutVisibility && clearTimeout(hideTimeoutVisibility)

      popoverElement.removeClass('popover-visible')
      popoverElement.removeClass('popover-shown')

      preparePopover()
    }, hoverDelayTime)
  }, function () {
    showTimeoutVisibility && clearTimeout(showTimeoutVisibility)
    showTimeoutAnimation && clearTimeout(showTimeoutAnimation)
    hoverDelayTimeout && clearTimeout(hoverDelayTimeout)

    currentLinkElement = null
    //hidePopover()
  })
}
