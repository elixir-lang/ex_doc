import { qs } from './helpers'

const CONTENT_SELECTOR = '.content'
const CONTENT_INNER_SELECTOR = '.content-inner'

/**
 * Runs some general modifiers on the documentation content.
 */
export function initialize () {
  fixLinks()
  fixSpacebar()
}

/**
 * Removes underline from links that have nested code or images.
 */
function fixLinks () {
  qs(CONTENT_SELECTOR).querySelectorAll('a').forEach(anchor => {
    if (anchor.querySelector('code, img')) {
      anchor.classList.add('no-underline')
    }
  })
}

/**
 * Focuses the content element.
 *
 * This is required so that the space bar (and similar key bindings)
 * work as soon as you visit a module's documentation. Without this,
 * the user would be forced to first click on the content element
 * before these keybindings worked.
 */
function fixSpacebar () {
  qs(CONTENT_INNER_SELECTOR).setAttribute('tabindex', -1)
  qs(CONTENT_INNER_SELECTOR).focus()
}
