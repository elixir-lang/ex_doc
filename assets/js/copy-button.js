import copyButton from './handlebars/templates/copy-button.handlebars'
import { qsAll } from './helpers'

/**
 * Initializes copy buttons.
 */
export function initialize () {
  if ('clipboard' in navigator) {
    addCopyButtons()
  }
}

/**
 * Find pre tags, add copy buttons, copy <code> content on click.
 */
function addCopyButtons () {
  Array.from(qsAll('pre')).forEach(pre => pre.insertAdjacentHTML('afterbegin', copyButton()))

  Array.from(qsAll('.copy-button')).forEach(button =>
    button.addEventListener('click', () =>
      navigator.clipboard.writeText(button.parentElement.querySelector('code').textContent)
    )
  )
}
