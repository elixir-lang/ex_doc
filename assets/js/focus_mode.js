// Dependencies
// ------------

import $ from 'jquery'

// Constants
// ---------

const body = 'body'

function escapeSlashes (selector) {
  return selector.replace('/', '\\/').replace(':', '\\:').replace('?', '\\?')
}

function focusFromHash () {
  const params = new URLSearchParams(window.location.search)

  if (!params.has('focused')) { return }

  const hash = escapeSlashes(window.location.hash)
  const infoElement = $(`${hash}.detail`)

  // .detail-link
  // .view-source
  if (infoElement.length <= 0) { return }

  $(body).html(`<div class="content-inner">${infoElement.html()}</div>`)
  $(body).addClass('focus-mode')
  $('.detail-link').remove()
  $('.view-source').remove()
}

// Public Methods
// --------------

export function initialize () {
  focusFromHash()
}
