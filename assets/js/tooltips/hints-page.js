// Dependencies
// ------------

import {extractModuleHint, extractFunctionHint} from './hints-extraction'
import $ from 'jquery'

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
  const content = $(contentInner)
  let hint = null

  if (!params.has('hint')) { return }

  const infoElement = descriptionElementFromHash(hash)

  if (infoElement && infoElement.length > 0) {
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
  return $(contentInner).find('#moduledoc').length > 0
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

  hash = decodeURI(hash)
  hash = $.escapeSelector(hash)

  if (!hash) { return null }

  const mainSelector = $(`#${hash}.detail`)

  if (mainSelector.length > 0) {
    return mainSelector
  } else {
    return $(`.detail > span#${hash}`).parent()
  }
}

/**
 * Grabs project version name from the meta tag.
 *
 * @returns {string} Project version name (ie. "Elixir v1.2.3")
 */
function getProjectVersion () {
  return $(projectMetaTag).attr('content')
}

// Public Methods
// --------------

export function initialize () {
  $(document).ready(function () {
    sendHint()
  })
}
