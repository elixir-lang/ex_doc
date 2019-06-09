// Dependencies
// ------------

import $ from 'jquery'

// Constants
// ---------
const popoverable = '.content a code' //, .signature .specs a
const popoverSelector = '#popover'
const popoverIframeSelector = '#popover .popover-iframe'
const contentInner = 'body .content-inner'
const popoverWidth = 500
let popoverHeight = null
let popoverElement = null
let currentLinkElement = null
let currentRequestId = null
let showTimeoutVisibility = null
let showTimeoutAnimation = null
let hideTimeoutVisibility = null

function updatePopoverPosition () {
  if (!currentLinkElement) { return }

  const popoverElement = $(popoverSelector)

  let popoverableBoundingRect = currentLinkElement[0].getBoundingClientRect()
  let contentInnerBoundingRect = $(contentInner)[0].getBoundingClientRect()

  console.log("rect1", popoverableBoundingRect)
  console.log("rect inner", contentInnerBoundingRect)

  popoverHeight = popoverElement[0].getBoundingClientRect().height

  console.log("popoverHeight", popoverHeight)

  const rect = {
    top: popoverableBoundingRect.top - contentInnerBoundingRect.top,
    bottom: popoverableBoundingRect.bottom - contentInnerBoundingRect.top,
    left: popoverableBoundingRect.left - contentInnerBoundingRect.left,
    right: popoverableBoundingRect.right - contentInnerBoundingRect.left,
    x: popoverableBoundingRect.x - contentInnerBoundingRect.x,
    y: popoverableBoundingRect.y - contentInnerBoundingRect.y,
    width: popoverableBoundingRect.width,
    height: popoverableBoundingRect.height
  }

  console.log("rect2", rect)

  let space = {
    left: popoverableBoundingRect.x,
    right: window.innerWidth - popoverableBoundingRect.x + rect.width,
    top: rect.y - window.scrollY,
    bottom: window.innerHeight - (rect.y - window.scrollY) + rect.height
  }



  console.log("space", space)

  if (space.bottom > popoverHeight + 50) {
    popoverElement.css('top', rect.bottom + 10)
  } else {
    popoverElement.css('top', rect.top - popoverHeight - 10)
  }

  if (space.left + popoverWidth < window.innerWidth) {
    popoverElement.css('left', rect.left)
    popoverElement.css('right', 'auto')
  } else {
    popoverElement.css('left', rect.right - popoverWidth)
    popoverElement.css('right', 'auto')
  }
}

function loadPopover () {
  console.log('load popover')
  if (!currentLinkElement) { return }

  const href = currentLinkElement.attr('href')

  if (!href) { return }

  // TODO: replace hash with full url

  const focusedHref = href.replace('.html', '.html?focused=true&requestId=' + currentRequestId)
  // TODO: Better reload
  $(popoverIframeSelector).attr('src', '')
  $(popoverIframeSelector).attr('src', focusedHref)
}

function showPopover (html) {
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

  showPopover(event.data.elementHTML)
}

function uid () {
  return Math.random().toString(36).substr(2, 9)
}

// Public Methods
// --------------

export function initialize () {
  window.addEventListener('message', receivePopupMessage, false)

  $(contentInner).append('<div id="popover"><div class="popover-body"></div><iframe class="popover-iframe"></iframe></div>')

  $(popoverable).hover(function () {
    popoverElement = $(popoverSelector)

    if (window.innerWidth < 768 || window.innerHeight < 400) {
      return
    }

    if (hideTimeoutVisibility) {
      clearTimeout(hideTimeoutVisibility)
      popoverElement.removeClass('popover-visible')
    }

    currentLinkElement = $(this).parent()
    currentRequestId = uid()

    loadPopover()
  }, function () {
    showTimeoutVisibility && clearTimeout(showTimeoutVisibility)
    showTimeoutAnimation && clearTimeout(showTimeoutAnimation)

    currentLinkElement = null
    hidePopover()
  })
}
