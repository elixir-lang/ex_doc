// Dependencies
// ------------

import $ from 'jquery'

// Constants
// ---------
const popoverable = '.content a code' //, .signature .specs a
const popoverSelector = '#popover'
const popoverIframeSelector = '#popover .popover-iframe'
const contentInner = 'body .content-inner'
const popoverHeight = 150
const popoverWidth = 500
let showTimeoutVisibility = null
let showTimeoutAnimation = null
let hideTimeoutVisibility = null

function updatePopoverPosition (linkElement) {
  const popoverElement = $(popoverSelector)

  let popoverableBoundingRect = linkElement[0].getBoundingClientRect()
  let contentInnerBoundingRect = $(contentInner)[0].getBoundingClientRect()

  console.log("rect1", popoverableBoundingRect)
  console.log("rect inner", contentInnerBoundingRect)

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
    popoverElement.css('top', rect.top - 30 - popoverHeight)
  }

  if (space.left + popoverWidth < window.innerWidth) {
    popoverElement.css('left', rect.left)
    popoverElement.css('right', 'auto')
  } else {
    popoverElement.css('left', rect.right - popoverWidth)
    popoverElement.css('right', 'auto')
  }
}

function loadPopover (linkElement) {
  const href = linkElement.attr('href')

  if (!href) { return }

  const focusedHref = href.replace('.html', '.html?focused=true&_t=' + Date.now())
  $(popoverIframeSelector).attr('src', focusedHref)

  updatePopoverPosition(linkElement)
}

function showPopover (html) {
  const popoverElement = $(popoverSelector)
  popoverElement.addClass('popover-visible')
  popoverElement.find('.popover-body').html(html)
  showTimeoutAnimation = setTimeout(() => {
    popoverElement.addClass('popover-shown')
  }, 10)
}

function hidePopover () {
  const popoverElement = $(popoverSelector)
  popoverElement.removeClass('popover-shown')
  hideTimeoutVisibility = setTimeout(() => {
    popoverElement.removeClass('popover-visible')
  }, 300)
}

function receivePopupMessage (event) {
  console.log('receivePopupMessage', event)
  if (event.data.ready && event.data.ready === true) {
    hideTimeoutVisibility && clearTimeout(hideTimeoutVisibility)
    showPopover(event.data.elementHTML)
  }
}

// Public Methods
// --------------

export function initialize () {
  window.addEventListener('message', receivePopupMessage, false)

  $(contentInner).append('<div id="popover"><div class="popover-body"></div><iframe class="popover-iframe"></iframe></div>')

  $(popoverable).hover(function () {
    if (window.innerWidth < 768 || window.innerHeight < 400) {
      return
    }

    const linkElement = $(this).parent()
    loadPopover(linkElement)
  }, function () {
    showTimeoutVisibility && clearTimeout(showTimeoutVisibility)
    showTimeoutAnimation && clearTimeout(showTimeoutAnimation)

    hidePopover()
  })
}
