import { extractModuleHint, extractFunctionHint } from './hints'
import { getCurrentPageSidebarType, descriptionElementFromHash, getLocationHash, getProjectNameAndVersion, qs } from '../helpers'

const CONTENT_INNER_SELECTOR = '.content-inner'

/**
 * Checks the URL query parameter for a hint request (`?hint=true`),
 * and when present extracts the relevant hint from the page content
 * and sends to the parent window as an event.
 */
export function initialize () {
  if (shouldSendHint()) {
    const hint = buildHint()
    if (hint) {
      sendHintToParentWindow(hint)
    }
  }
}

function shouldSendHint () {
  const params = new URLSearchParams(window.location.search)
  return params.has('hint') && window.self !== window.parent
}

function sendHintToParentWindow (hint) {
  const href = window.location.href
  const message = { hint, href }
  window.parent.postMessage(message, '*')
}

function buildHint () {
  const infoElement = descriptionElementFromHash(getLocationHash())
  const content = qs(CONTENT_INNER_SELECTOR)

  if (infoElement) {
    const hint = extractFunctionHint(infoElement)
    return withVersion(hint)
  } else if (isModulePage()) {
    const hint = extractModuleHint(content)
    return withVersion(hint)
  }

  return null
}

function withVersion (hint) {
  const version = getProjectNameAndVersion()
  return { ...hint, version }
}

/**
 * Checks if the current page is dedicated to a module.
 * Note: tasks are treated in the same way as modules.
 *
 * @returns {Boolean} Returns true if the current page documents a module.
 */
function isModulePage () {
  return ['modules', 'tasks'].includes(getCurrentPageSidebarType())
}
