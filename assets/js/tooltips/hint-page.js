import { extractModuleHint, extractFunctionHint } from './hints'
import { getCurrentPageSidebarType, descriptionElementFromHash, getProjectNameAndVersion, qs } from '../helpers'
import { isEmbedded, isHint } from '../globals'

/**
 * Checks the URL query parameter for a hint request (`?hint=true`),
 * and when present extracts the relevant hint from the page content
 * and sends to the parent window as an event.
 */

if (isHint && isEmbedded) {
  const infoElement = descriptionElementFromHash()

  const hint = infoElement
    ? extractFunctionHint(infoElement)
    // Tasks are modules.
    : ['modules', 'tasks'].includes(getCurrentPageSidebarType())
        ? extractModuleHint(qs('.content-inner'))
        : null

  if (hint) {
    // Send hint to parent.
    const message = {
      hint: {
        ...hint,
        version: getProjectNameAndVersion()
      },
      href: window.location.href
    }
    window.parent.postMessage(message, '*')
  }

  // Empty content to prevent other modules doing work.
  qs('.content-inner')?.replaceChildren()
}
