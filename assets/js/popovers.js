// Dependencies
// ------------

import $ from 'jquery'

// Constants
// ---------
const popoverable = '.content a.no-underline' //, .signature .specs a
const popoverSelector = '#popover'
const popoverIframeSelector = '#popover .popover-iframe'
const body = 'body'
let showTimeout = null

function showPopover (element) {
  const popoverElement = $(popoverSelector)
  const popoverableCoordinates = element[0].getBoundingClientRect()
  const focusedHref = element.attr('href').replace('.html', '.html?focused=true&_t=' + Date.now())
  console.log('focused href', focusedHref)

  $(popoverIframeSelector).attr('src', focusedHref)
  //$(popoverIframeSelector)[0].contentDocument.location.reload(true)

  popoverElement.css('top', popoverableCoordinates.top + popoverableCoordinates.height + 10)
  popoverElement.css('left', popoverableCoordinates.left)

  showTimeout = setTimeout(() => {
    popoverElement.addClass('popover-visible')
    setTimeout(() => {
      popoverElement.addClass('popover-shown')
      console.log(popoverElement)
    }, 10)
  }, 300)
}

function hidePopover () {
  const popoverElement = $(popoverSelector)
  popoverElement.removeClass('popover-visible')
  popoverElement.removeClass('popover-shown')
}

// Public Methods
// --------------

export function initialize () {
  $(body).append('<div id="popover"><iframe class="popover-iframe"></iframe></div>')

  $(popoverable).hover(function () {
    showPopover($(this))
  }, function () {
    showTimeout && clearTimeout(showTimeout)
    hidePopover()
  })
}
