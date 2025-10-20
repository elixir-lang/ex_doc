/**
 * Initializes markdown copy functionality.
 */
export function initialize () {
  console.log('Initializing copy-markdown functionality')
  
  if ('clipboard' in navigator) {
    // Make the copyMarkdown function globally available
    window.copyMarkdown = copyMarkdown
    console.log('copyMarkdown function attached to window')
  } else {
    console.warn('Clipboard API not available')
  }
}

/**
 * Copies the markdown version of the current page to clipboard.
 * @param {string} markdownPath - The path to the markdown file
 */
async function copyMarkdown (markdownPath) {
  console.log('copyMarkdown called with path:', markdownPath)
  
  try {
    // Check if clipboard API is available
    if (!navigator.clipboard) {
      throw new Error('Clipboard API not available')
    }

    // Construct the URL for the markdown file
    // We need to replace the current filename with the markdown version
    const currentUrl = new URL(window.location.href)
    const baseUrl = currentUrl.origin + currentUrl.pathname.replace(/\/[^/]*$/, '')
    const markdownUrl = `${baseUrl}/markdown/${markdownPath}`
    
    console.log('Fetching markdown from:', markdownUrl)

    // Fetch the markdown content
    const response = await fetch(markdownUrl)

    if (!response.ok) {
      throw new Error(`Failed to fetch markdown: ${response.status}`)
    }

    const markdownContent = await response.text()
    console.log('Markdown content length:', markdownContent.length)

    // Copy to clipboard with fallback
    await copyToClipboard(markdownContent)

    // Show success feedback
    showCopyFeedback('Markdown copied!')
    console.log('Markdown copied successfully')

  } catch (error) {
    console.error('Failed to copy markdown:', error)
    showCopyFeedback('Failed to copy markdown: ' + error.message, true)
  }
}

/**
 * Copies text to clipboard with fallback for older browsers.
 * @param {string} text - The text to copy
 */
async function copyToClipboard(text) {
  // Try modern clipboard API first
  if (navigator.clipboard && window.isSecureContext) {
    await navigator.clipboard.writeText(text)
  } else {
    // Fallback for older browsers or non-secure contexts
    const textArea = document.createElement('textarea')
    textArea.value = text
    textArea.style.position = 'fixed'
    textArea.style.left = '-999999px'
    textArea.style.top = '-999999px'
    document.body.appendChild(textArea)
    textArea.focus()
    textArea.select()
    
    return new Promise((resolve, reject) => {
      const successful = document.execCommand('copy')
      document.body.removeChild(textArea)
      
      if (successful) {
        resolve()
      } else {
        reject(new Error('Unable to copy to clipboard'))
      }
    })
  }
}

/**
 * Shows feedback when copying markdown.
 * @param {string} message - The message to show
 * @param {boolean} isError - Whether this is an error message
 */
function showCopyFeedback (message, isError = false) {
  // Create or update a feedback element
  let feedback = document.getElementById('markdown-copy-feedback')

  if (!feedback) {
    feedback = document.createElement('div')
    feedback.id = 'markdown-copy-feedback'
    feedback.style.cssText = `
      position: fixed;
      top: 20px;
      right: 20px;
      padding: 10px 16px;
      border-radius: 4px;
      font-size: 14px;
      font-weight: 500;
      color: white;
      z-index: 10000;
      transition: opacity 0.3s ease;
    `
    document.body.appendChild(feedback)
  }

  feedback.textContent = message
  feedback.style.backgroundColor = isError ? '#dc2626' : '#059669'
  feedback.style.opacity = '1'

  // Hide after 3 seconds
  setTimeout(() => {
    feedback.style.opacity = '0'
    setTimeout(() => {
      if (feedback.parentNode) {
        feedback.parentNode.removeChild(feedback)
      }
    }, 300)
  }, 3000)
}