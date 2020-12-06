import { qsAll } from './helpers'

const HIGHLIGHT_CLASS = 'hll'

/**
 * Sets up dynamic behaviour for code blocks processed with *makeup*.
 */
export function initialize () {
  initializeDelimitersHighlighting()
}

// Hovering over a delimiter (bracket, parenthesis, do/end)
// highlights the relevant pair of delimiters.
function initializeDelimitersHighlighting () {
  const delimiters = qsAll('[data-group-id]')

  delimiters.forEach(delimiter => {
    const groupId = delimiter.getAttribute('data-group-id')

    delimiter.addEventListener('mouseenter', event => {
      toggleDelimitersHighlight(groupId, true)
    })

    delimiter.addEventListener('mouseleave', event => {
      toggleDelimitersHighlight(groupId, false)
    })
  })
}

function toggleDelimitersHighlight (groupId, force) {
  const delimiters = qsAll(`[data-group-id="${groupId}"]`)
  delimiters.forEach(delimiter => {
    delimiter.classList.toggle(HIGHLIGHT_CLASS, force)
  })
}
