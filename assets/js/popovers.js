// Dependencies
// ------------

import $ from 'jquery'

// Constants
// ---------
const popoverable = '.content a.no-underline' //, .signature .specs a
const popoverSelector = '#popover'
const popoverIframeSelector = '#popover .popover-iframe'
const body = 'body'
const popoverHeight = 150
const popoverWidth = 500
let showTimeoutVisibility = null
let showTimeoutAnimation = null
let hideTimeoutVisibility = null;

function showPopover (element) {
  const popoverElement = $(popoverSelector)
  const popoverableBoundingRect = element[0].getBoundingClientRect()
  const focusedHref = element.attr('href').replace('.html', '.html?focused=true&_t=' + Date.now())
  $(popoverIframeSelector).attr('src', focusedHref)

  console.log(popoverableBoundingRect)

  let space = {
    left: popoverableBoundingRect.x,
    right: window.innerWidth - popoverableBoundingRect.x + popoverableBoundingRect.width,
    top: popoverableBoundingRect.y,
    bottom: window.innerHeight - popoverableBoundingRect.y + popoverableBoundingRect.height
  }

  console.log(space)

  if (space.bottom > popoverHeight + 50) {
    popoverElement.css('top', popoverableBoundingRect.bottom + 10)
  } else {
    popoverElement.css('top', popoverableBoundingRect.top - 30 - popoverHeight)
  }

  if (space.left + popoverWidth < window.innerWidth) {
    popoverElement.css('left', popoverableBoundingRect.left)
    popoverElement.css('right', 'auto')
  } else {
    popoverElement.css('left', popoverableBoundingRect.right - popoverWidth)
    popoverElement.css('right', 'auto')
  }

  showTimeoutVisibility = setTimeout(() => {
    popoverElement.addClass('popover-visible')
    showTimeoutAnimation = setTimeout(() => {
      popoverElement.addClass('popover-shown')
      console.log(popoverElement)
    }, 10)
  }, 300)
}

function hidePopover () {
  const popoverElement = $(popoverSelector)
  popoverElement.removeClass('popover-shown')
  hideTimeoutVisibility = setTimeout(() => {
    popoverElement.removeClass('popover-visible')
  }, 300)
}

// Public Methods
// --------------

export function initialize () {
  $(body).append('<div id="popover"><iframe class="popover-iframe"></iframe></div>')

  $(popoverable).hover(function () {
    if (window.innerWidth < 768 || window.innerHeight < 400) {
      return
    }

    hideTimeoutVisibility && clearTimeout(hideTimeoutVisibility)
    showPopover($(this))
  }, function () {
    showTimeoutVisibility && clearTimeout(showTimeoutVisibility)
    showTimeoutAnimation && clearTimeout(showTimeoutAnimation)
    hidePopover()
  })
}
