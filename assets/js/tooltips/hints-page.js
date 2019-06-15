// Dependencies
// ------------

import {extractTypeSummary, extractModuleSummary, extractFunctionSummary} from './hints-extraction'
import $ from 'jquery'
import find from 'lodash.find'

// Constants
// ---------

const contentInner = '.content-inner'
const message = {summary: {}, ready: false, requestId: null}
const typespecs = {
  pathnameEnd: '/typespecs.html',
  categories: [
    { name: 'basicType', description: 'Basic type', hash: '#basic-types', detailsAvailable: false },
    { name: 'literal', description: 'Literal', hash: '#literals', detailsAvailable: false },
    { name: 'builtInType', description: 'Built-in type', hash: '#built-in-types', detailsAvailable: true }
  ]
}

function descriptionElementFromHash (hash) {
  if (!hash) { return null }
  hash = hash.substr(1) // removes the `#` in `#hash`

  if (!hash) { return null }
  hash = $.escapeSelector(hash)

  if (!hash) { return null }

  return $(`#${hash}.detail`)
}

function sendHint () {
  const params = new URLSearchParams(window.location.search)
  const requestId = params.get('requestId')
  const hash = window.location.hash
  const content = $(contentInner)
  let summary = null

  if (!params.has('hint')) { return }

  if (!requestId) { return }

  const infoElement = descriptionElementFromHash(hash)
  const typeCategory = typeCategoryFromHash(hash)

  if (infoElement && infoElement.length > 0) {
    summary = extractFunctionSummary(infoElement)
  } else if (isTypesPage(params)) {
    const typeName = params.get('typeName')
    const category = typeCategory(hash)
    summary = extractTypeSummary(content, typeName, category)
  } else if (isModulePage()) {
    summary = extractModuleSummary(content)
  }

  console.log("focus_mode - got summary", summary)

  if (!summary) { return }

  postMessage(summary, requestId)
}

function postMessage (summary, requestId) {
  console.log('focus_mod - sending messages', summary)
  if (window.self !== window.parent) {
    message.summary = summary
    message.ready = true
    message.requestId = requestId
    window.parent.postMessage(message, '*')
  }
}

/**
 * Checks if the current page is dedicated to an Elixir module.
 *
 * @returns {boolean} `true` if current page contains module documentation.
 */
function isModulePage () {
  return $(contentInner).find('#moduledoc').length > 0
}

/**
 * Checks if the current page is the typespecs page and if we're requesting type info.
 *
 * @param {Object} params URLSearchParams object, parsed parameters from the URL
 * @param {(string|null)} [moduleId=null] Id of the parent module. If null it means we are serializing the parent module info.
 *
 * @returns {boolean} `true` if current page is the typespecs page and a type is being requested
 */
function isTypesPage (params) {
  const isThisTypespecsPage = window.location.pathname.indexOf(typespecs.pathnameEnd) > 0
  const isTypesHashPresent = !!typeCategoryFromHash(window.location.hash)
  const isTypeRequested = !!params.get('typeName')

  console.log("focus_mode - isTypesPage", isThisTypespecsPage, isTypesHashPresent, isTypeRequested)

  return isThisTypespecsPage && isTypesHashPresent && isTypeRequested
}

function typeCategoryFromHash (hash) {
  return find(typespecs.categories, {hash: hash})
}

// Public Methods
// --------------

export function initialize () {
  $(document).ready(function () {
    sendHint()
  })
}
