import { qsAll } from './helpers'
import { showToast } from './toast'

/**
 * Initializes copy markdown links.
 */

window.addEventListener('exdoc:loaded', initialize)

function initialize () {
  if (!('clipboard' in navigator)) return

  qsAll('a.copy-markdown').forEach(link => {
    link.addEventListener('click', handleCopyMarkdownClick)
  })
}

/**
 * Handles clicks on copy markdown links.
 *
 * If Ctrl/Cmd is held, allows normal link behavior.
 * Otherwise, attempts to fetch and copy the markdown to clipboard.
 *
 * @param {MouseEvent} event
 */
function handleCopyMarkdownClick (event) {
  if (event.ctrlKey || event.metaKey) {
    return
  }

  event.preventDefault()
  const link = event.currentTarget
  const markdownUrl = link.href

  // Use ClipboardItem with a promise for Safari compatibility as it
  // requires the clipboard API to be called synchronously during user gesture
  const clipboardItem = new ClipboardItem({
    'text/plain': fetch(markdownUrl)
      .then(response => {
        if (!response.ok) {
          throw new Error('Failed to fetch markdown')
        }
        return response.text()
      })
      .then(markdown => {
        return new Blob([markdown], { type: 'text/plain' })
      })
  })

  navigator.clipboard.write([clipboardItem])
    .then(() => {
      showToast('Page copied as Markdown to clipboard')
    })
    .catch((error) => {
      console.log('Copying Markdown failed:', error)

      const shouldOpen = window.confirm(
        'Could not copy to clipboard. Do you want to open the Markdown page instead?'
      )
      if (shouldOpen) {
        window.location.href = markdownUrl
      }
    })
}
