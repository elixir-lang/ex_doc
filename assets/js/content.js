import { qs, qsAll } from './helpers'
import { settingsStore } from './settings-store'

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
  fixBlockquotes()
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
 * Add CSS classes to `blockquote` elements when those are used to
 * support admonition text blocks
 */
function fixBlockquotes () {
  const classes = ['warning', 'info', 'error', 'neutral', 'tip']

  classes.forEach(element => {
    qsAll(`blockquote h3.${element}, blockquote h4.${element}`).forEach(header => {
      header.closest('blockquote').classList.add(element)
    })
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

/**
 * Updates "Run in Livebook" badges to link to a notebook
 * corresponding to the current documentation page.
 */
function setLivebookBadgeUrl () {
  const path = window.location.pathname
  const notebookPath = path.replace(/(\.html)?$/, '.livemd')
  const notebookUrl = new URL(notebookPath, window.location.href).toString()

  settingsStore.getAndSubscribe(settings => {
    const targetUrl =
      settings.livebookUrl
        ? getLivebookImportUrl(settings.livebookUrl, notebookUrl)
        : getLivebookDevRunUrl(notebookUrl)

    for (const anchor of qsAll(LIVEBOOK_BADGE_ANCHOR_SELECTOR)) {
      anchor.href = targetUrl
    }
  })
}

function getLivebookDevRunUrl (notebookUrl) {
  return `https://livebook.dev/run?url=${encodeURIComponent(notebookUrl)}`
}

function getLivebookImportUrl (livebookUrl, notebookUrl) {
  return `${livebookUrl}/import?url=${encodeURIComponent(notebookUrl)}`
}

// Check if the device width is below a certain threshold (e.g., 768px for mobile)
document.addEventListener('click', function (e) {
  if (window.innerWidth <= 768) {
    const target = e.target.closest('a[href^="#"]')
    if (target) {
      e.preventDefault()
      const targetId = target.getAttribute('href').substring(1)
      const targetElement = document.getElementById(targetId)
      if (targetElement) {
        const offset = 45 // Adjust this offset as needed
        const targetPosition = targetElement.getBoundingClientRect().top + window.scrollY - offset
        window.scrollTo({
          top: targetPosition,
          behavior: 'smooth'
        })
      }
    }
  }
})
