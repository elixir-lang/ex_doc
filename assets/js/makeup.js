import { qsAll } from './helpers'

const HIGHLIGHT_CLASS = 'hll'

/**
 * Sets up dynamic behaviour for code blocks processed with *makeup*.
 */

window.addEventListener('swup:page:view', initialize)
initialize()

export function initialize () {
  // Hovering over a delimiter (bracket, parenthesis, do/end)
  // highlights the relevant pair of delimiters.
  qsAll('[data-group-id]').forEach(delimiter => {
    delimiter.addEventListener('mouseenter', toggleDelimitersHighlight)
    delimiter.addEventListener('mouseleave', toggleDelimitersHighlight)
  })
}

/** @param {MouseEvent} event */
function toggleDelimitersHighlight (event) {
  const element = event.currentTarget
  const force = event.type === 'mouseenter'
  const groupId = element.getAttribute('data-group-id')
  element.parentElement.querySelectorAll(`[data-group-id="${groupId}"]`).forEach(delimiter => {
    delimiter.classList.toggle(HIGHLIGHT_CLASS, force)
  })
}
