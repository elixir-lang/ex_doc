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
const popoverWidth = 500
const minBottomSpacing = 50
const spacingAroundLink = 10
const hoverDelayTime = 150
const disabledDestinations = ['typespecs.html']
let popoverHeight = null
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

  let popoverableBoundingRect = currentLinkElement[0].getBoundingClientRect()
  let contentInnerBoundingRect = $(contentInner)[0].getBoundingClientRect()

  popoverHeight = popoverElement[0].getBoundingClientRect().height

  const absoluteBoundingRect = {
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
    top: absoluteBoundingRect.y - window.scrollY,
    bottom: window.innerHeight - (absoluteBoundingRect.y - window.scrollY) + absoluteBoundingRect.height
  }

  if (space.bottom > popoverHeight + minBottomSpacing) {
    popoverElement.css('top', absoluteBoundingRect.bottom + spacingAroundLink)
  } else {
    popoverElement.css('top', absoluteBoundingRect.top - popoverHeight - spacingAroundLink)
  }

  if (space.left + popoverWidth < window.innerWidth) {
    popoverElement.css('left', absoluteBoundingRect.left)
    popoverElement.css('right', 'auto')
  } else {
    popoverElement.css('left', absoluteBoundingRect.right - popoverWidth)
    popoverElement.css('right', 'auto')
  }
}

function loadPopover () {
  updatePopoverPosition()

  if (!currentLinkElement) { return }

  let href = currentLinkElement.attr('href')

  if (!href) { return }

  if (linkDisabled(href)) { return }

  if (href.charAt(0) === '#') {
    href = `${window.location.pathname}${href}`
  }

  const focusedHref = rewriteHref(href)
  $(popoverIframeSelector).attr('src', focusedHref)
}

function showPopover (summary) {
  const html = popoverTemplate({
    isTypePage: summary.type === 'page',
    isTypeFunction: summary.type === 'function',
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
  return href.replace('.html', `.html?focused=true&requestId=${currentRequestId}`)
}

function linkDisabled (href) {
  return disabledDestinations.reduce(function (isDisabled, linkFragment) {
    const currentDisabled = (href.indexOf(linkFragment) === 0 || href.indexOf(`/${linkFragment}`) >= 0)

    if (currentDisabled) {
      return true
    } else {
      return isDisabled
    }
  }, false)
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

      loadPopover()
    }, hoverDelayTime)
  }, function () {
    showTimeoutVisibility && clearTimeout(showTimeoutVisibility)
    showTimeoutAnimation && clearTimeout(showTimeoutAnimation)
    hoverDelayTimeout && clearTimeout(hoverDelayTimeout)

    currentLinkElement = null
    //hidePopover()
  })
}
