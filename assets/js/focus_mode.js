// Dependencies
// ------------

import $ from 'jquery'

// Constants
// ---------

const body = 'body'
const contentInner = '.content-inner'
const message = {summary: '', ready: false, requestId: null}

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

function postMessage (summary, requestId) {
  if (window.self !== window.parent) {
    message.summary = summary
    message.ready = true
    message.requestId = requestId
    window.parent.postMessage(message, '*')
  }
}

function prepareFunctionSummary (element) {
  const signatureSpecs = element.find('h1 .specs').text()
  element.find('h1 > *').remove()
  const signatureTitle = element.find('h1').text()
  const description = element.find('.docstring > p:first').text()

  return {
    type: 'function',
    signatureTitle: signatureTitle,
    signatureSpecs: signatureSpecs,
    description: description.trim()
  }
}

function preparePageSummary () {
  let content = $(contentInner)
  content.find('h1:first > *').remove()

  return {
    type: 'page',
    title: content.find('h1:first').text(),
    description: content.find('#moduledoc p:first').text().trim()
  }
}

// Public Methods
// --------------

export function initialize () {
  focusFromHash()
}
