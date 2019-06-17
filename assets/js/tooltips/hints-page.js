// Dependencies
// ------------

import {extractTypeHint, extractModuleHint, extractFunctionHint} from './hints-extraction'
import $ from 'jquery'
import find from 'lodash.find'

// Constants
// ---------

const contentInner = '.content-inner'
const message = {hint: {}, ready: false, requestId: null}
const typespecs = {
  pathnameEnd: '/typespecs.html',
  categories: [
    { name: 'basicType', description: 'Basic type', hash: '#basic-types', detailsAvailable: false },
    { name: 'literal', description: 'Literal', hash: '#literals', detailsAvailable: false },
    { name: 'builtInType', description: 'Built-in type', hash: '#built-in-types', detailsAvailable: true }
  ]
}

/**
 *  Will try to extract hint info, and if successful triggers a message to the parent page.
 */
function sendHint () {
  const params = new URLSearchParams(window.location.search)
  const requestId = params.get('requestId')
  const hash = window.location.hash
  const content = $(contentInner)
  let hint = null

  if (!params.has('hint')) { return }

  if (!requestId) { return }

  const infoElement = descriptionElementFromHash(hash)
  const typeCategory = typeCategoryFromHash(hash)

  if (infoElement && infoElement.length > 0) {
    hint = extractFunctionHint(infoElement)
  } else if (isTypesPage(params)) {
    const typeName = params.get('typeName')
    hint = extractTypeHint(content, typeName, typeCategory)
  } else if (isModulePage()) {
    hint = extractModuleHint(content)
  }

  if (!hint) { return }

  postMessage(hint, requestId)
}

/**
 * Sends a message (containing everything needed to display a tooltip hint) to the parent page.
 */
function postMessage (hint, requestId) {
  if (window.self !== window.parent) {
    message.hint = hint
    message.ready = true
    message.requestId = requestId
    window.parent.postMessage(message, '*')
  }
}

/**
 * Checks if the current page is dedicated to a module.
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
 *
 * @returns {boolean} `true` if current page is the typespecs page and a type is being requested
 */
function isTypesPage (params) {
  const isThisTypespecsPage = window.location.pathname.indexOf(typespecs.pathnameEnd) > 0
  const isTypesHashPresent = !!typeCategoryFromHash(window.location.hash)
  const isTypeRequested = !!params.get('typeName')

  return isThisTypespecsPage && isTypesHashPresent && isTypeRequested
}

/**
 * Finds to which category type specified in the hash belongs to.
 * We get category information back, which let us know which type we're dealing with and how to
 * prepare a hint for it.
 *
 * @param {string} hash ie. `#basic-types`
 *
 * @returns {object} Obect containing information about the selcted category
 */
function typeCategoryFromHash (hash) {
  return find(typespecs.categories, {hash: hash})
}

/**
 * Constructs a jQuery selector targeting an element
 *
 * @param {Object} params URLSearchParams object, parsed parameters from the URL
 *
 * @returns {object} jquery selector
 */
function descriptionElementFromHash (hash) {
  if (!hash) { return null }
  hash = hash.substr(1) // removes the `#` in `#hash`

  if (!hash) { return null }
  hash = $.escapeSelector(hash)

  if (!hash) { return null }

  return $(`#${hash}.detail`)
}

// Public Methods
// --------------

export function initialize () {
  $(document).ready(function () {
    sendHint()
  })
}
