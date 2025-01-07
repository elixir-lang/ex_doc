import { el, qsAll } from './helpers'

const buttonTemplate = el('button', {class: 'copy-button'})
buttonTemplate.innerHTML = '<svg role="img" aria-label="copy" viewBox="0 0 24 24" fill="currentColor"><path d="M0 0h24v24H0z" fill="none"/><path d="M16 1H4c-1.1 0-2 .9-2 2v14h2V3h12V1zm3 4H8c-1.1 0-2 .9-2 2v14c0 1.1.9 2 2 2h11c1.1 0 2-.9 2-2V7c0-1.1-.9-2-2-2zm0 16H8V7h11v14z"/></svg><span aria-live="polite"></span>'

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
  qsAll('pre:has(> code:first-child):not(:has(.copy-button))').forEach(pre => {
    const button = buttonTemplate.cloneNode(true)
    pre.appendChild(button)

    let timeout
    button.addEventListener('click', () => {
      const ariaLiveContent = button.querySelector('[aria-live]')
      clearTimeout(timeout)

      const text =
        Array.from(pre.querySelectorAll('code > *:not(.unselectable)'))
          .map(elem => elem.textContent)
          .join('')

      navigator.clipboard.writeText(text)
      button.classList.add('clicked')
      ariaLiveContent.innerHTML = 'Copied! &#x2713;'
      timeout = setTimeout(() => {
        button.classList.remove('clicked')
        ariaLiveContent.innerHTML = ''
      }, 3000)
    })
  })
}
