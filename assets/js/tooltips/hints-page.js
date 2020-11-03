// Dependencies
// ------------

import {extractModuleHint, extractFunctionHint} from './hints-extraction'
import {onDocumentReady, qs} from '../helpers'

// Constants
// ---------

const contentInner = '.content-inner'
const projectMetaTag = 'meta[name="project"]'
const message = {hint: {}, ready: false, href: ''}

/**
 *  Will try to extract hint info, and if successful triggers a message to the parent page.
 */
function sendHint () {
  const params = new URLSearchParams(window.location.search)
  const hash = window.location.hash
  const content = qs(contentInner)
  let hint = null

  if (!params.has('hint')) { return }

  const infoElement = descriptionElementFromHash(hash)

  if (infoElement) {
    hint = extractFunctionHint(infoElement)
  } else if (isModulePage()) {
    hint = extractModuleHint(content)
  }

  if (!hint) { return }

  hint.version = getProjectVersion()

  postMessage(hint, window.location.href)
}

/**
 * Sends a message (containing everything needed to display a tooltip hint) to the parent page.
 */
function postMessage (hint, href) {
  if (window.self !== window.parent) {
    message.hint = hint
    message.ready = true
    message.href = href
    window.parent.postMessage(message, '*')
  }
}

/**
 * Checks if the current page is dedicated to a module.
 *
 * @returns {boolean} `true` if current page contains module documentation.
 */
function isModulePage () {
  return qs(contentInner).querySelector('#moduledoc') != null
}

/**
 * Finds an element by a URL hash
 *
 * @param {String} hash the hash part of a URL
 *
 * @returns {Object} an element
 */
function descriptionElementFromHash (hash) {
  if (!hash) { return null }
  hash = hash.substr(1) // removes the `#` in `#hash`

  if (!hash) { return null }

  const mainSelector = document.getElementById(hash)

  if (mainSelector) {
    return mainSelector
  } else {
    return document.getElementById(hash).parentElement
  }
}

/**
 * Grabs project version name from the meta tag.
 *
 * @returns {string} Project version name (ie. "Elixir v1.2.3")
 */
function getProjectVersion () {
  return qs(projectMetaTag).getAttribute('content')
}

// Public Methods
// --------------

export function initialize () {
  onDocumentReady(() => sendHint())
}
