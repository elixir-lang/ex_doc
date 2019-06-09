// Dependencies
// ------------

import $ from 'jquery'

// Constants
// ---------

const body = 'body'
const contentInner = '.content-inner'
const message = {elementHTML: null, ready: false, requestId: null}

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
  const requestId = params.get('requestId')
  let summary = ''

  if (!params.has('focused')) { return }

  if (!requestId) { return }

  const infoElement = hashToElement(window.location.hash)

  if (!infoElement || infoElement.length <= 0) {
    summary = preparePageSummary()
  } else {
    summary = prepareFunctionSummary(infoElement)
  }

  $(document).ready(function () {
    postMessage(summary, requestId)
  })
}

function postMessage (elementHTML, requestId) {
  if (window.self !== window.parent) {
    message.elementHTML = elementHTML
    message.ready = true
    message.requestId = requestId
    window.parent.postMessage(message, '*')
  }
}

function prepareFunctionSummary (element) {
  element.find('.detail-link').remove()
  element.find('.signature a').remove()
  element.find('.docstring > *').not(':first').remove()
  return element.html()
}

function preparePageSummary () {
  let content = $(contentInner)
  let title = content.find('h1:first').text()
  let desc = content.find('#moduledoc p:first').text()
  return title + desc
}

// Public Methods
// --------------

export function initialize () {
  focusFromHash()
}
