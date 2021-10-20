import { qs, qsAll } from './helpers'

const CONTENT_SELECTOR = '.content'
const CONTENT_INNER_SELECTOR = '.content-inner'
const LIVEBOOK_BADGE_ANCHOR_SELECTOR = '.livebook-badge'

/**
 * Runs some general modifiers on the documentation content.
 */
export function initialize () {
  fixLinks()
  fixSpacebar()
  setLivebookBadgeUrl()
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

function setLivebookBadgeUrl () {
  const path = window.location.pathname
  const notebookPath = path.replace(/\.html$/, '.livemd')
  const notebookUrl = new URL(notebookPath, window.location.href).toString()
  const targetUrl = `https://livebook.dev/run?url=${encodeURIComponent(notebookUrl)}`

  for (const anchor of qsAll(LIVEBOOK_BADGE_ANCHOR_SELECTOR)) {
    anchor.href = targetUrl
  }
}
