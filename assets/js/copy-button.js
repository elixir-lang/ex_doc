import { qsAll } from './helpers'
import buttonHtml from './handlebars/templates/copy-button.html'

/** @type {HTMLButtonElement} */
let buttonTemplate

/**
 * Initializes copy buttons.
 */

window.addEventListener('exdoc:loaded', initialize)

function initialize () {
  if (!('clipboard' in navigator)) return

  qsAll('pre:has(> code:first-child):not(:has(.copy-button))').forEach(pre => {
    if (!buttonTemplate) {
      const div = document.createElement('div')
      div.innerHTML = buttonHtml
      buttonTemplate = div.firstChild
    }

    const button = buttonTemplate.cloneNode(true)
    pre.appendChild(button)

    let timeout
    button.addEventListener('click', () => {
      clearTimeout(timeout)

      const text =
        Array.from(pre.querySelectorAll('code > *'))
          .filter(elem => !elem.classList.contains('unselectable') || elem.textContent !== '$ ')
          .map(elem => elem.textContent)
          .join('')

      navigator.clipboard.writeText(text)
      button.classList.add('clicked')
      button.disabled = true
      timeout = setTimeout(() => {
        button.classList.remove('clicked')
        button.disabled = false
      }, 3000)
    })
  })
}
