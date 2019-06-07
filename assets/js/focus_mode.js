// Dependencies
// ------------

import $ from 'jquery'

// Constants
// ---------

const body = 'body'
const contentInner = '.content-inner'
const message = {elementHTML: null, ready: false}

function hashToElement (hash) {
  if (!hash) { return null }
  hash = hash.substr(1)

  if (!hash) { return null }
  hash = $.escapeSelector(hash)

  if (hash === '') { return null }

  return $(`#${hash}.detail`)
}

function focusFromHash () {
  const params = new URLSearchParams(window.location.search)

  if (!params.has('focused')) { return }

  const infoElement = hashToElement(window.location.hash)

  if (!infoElement || infoElement.length <= 0) { return }

  $(document).ready(function () {
    const summary = prepareSummary(infoElement)
    postMessage(summary)
  })
}

function postMessage (elementHTML) {
  if (window.self !== window.parent) {
    message.elementHTML = elementHTML
    message.ready = true
    window.parent.postMessage(message, '*')
  }
}

function prepareSummary (element) {
  element.find('.detail-link').remove()
  element.find('.signature a').remove()
  element.find('.docstring > *').not(':first').remove()
  return element.html()
}

// Public Methods
// --------------

export function initialize () {
  focusFromHash()
}
