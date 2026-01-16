import buttonHtml from './handlebars/templates/markdown-copy-button.html'
import { qsAll } from './helpers'

/** @type {HTMLButtonElement} */
let buttonTemplate

/**
 * Initializes copy-as-markdown buttons.
 * Fetches the .md version of the page and copies it to clipboard.
 * Only adds buttons if the markdown file exists.
 */

window.addEventListener('exdoc:loaded', initializeCopyMarkdownButtons)

function initializeCopyMarkdownButtons () {
  if (!('clipboard' in navigator)) {
    return
  }

  // Check if markdown formatter is enabled
  if (!window.__EXDOC__?.formatters?.includes('markdown')) {
    return
  }

  const markdownUrl = getMarkdownUrl()

  qsAll('.top-heading.heading-with-actions').forEach(container => {
    if (container.querySelector('.copy-markdown-button')) return

    if (!buttonTemplate) {
      const div = document.createElement('div')
      div.innerHTML = buttonHtml
      buttonTemplate = div.firstChild
    }

    const button = buttonTemplate.cloneNode(true)
    button.addEventListener('click', () => handleCopyMarkdown(button, markdownUrl))
    container.appendChild(button)
    console.log('Added copy-markdown button')
  })
}

function getMarkdownUrl () {
  const url = new URL(window.location.href)
  // Replace .html with markdown/filename.md
  // e.g., /doc/ExDoc.Markdown.html -> /doc/markdown/ExDoc.Markdown.md
  const markdownPath = url.pathname.replace(/([^/]+)\.html$/, 'markdown/$1.md')
  return url.origin + markdownPath
}

async function handleCopyMarkdown (button, markdownUrl) {
  const originalInnerHTML = button.innerHTML

  try {
    const response = await fetch(markdownUrl)

    if (!response.ok) {
      throw new Error(`Failed to fetch markdown: ${response.status}`)
    }

    const markdownContent = await response.text()

    await navigator.clipboard.writeText(markdownContent)

    button.classList.add('clicked')
    button.disabled = true

    setTimeout(() => {
      button.classList.remove('clicked')
      button.disabled = false
      button.innerHTML = originalInnerHTML
    }, 3000)
  } catch (error) {
    console.error('Failed to copy markdown:', error)
    button.disabled = false
  }
}
