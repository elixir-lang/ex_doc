/**
 * @typedef Hint
 * @type {Object}
 * @property {String} kind The type of whatever the hint concerns (e.g. 'function'). Use 'plain' for simple, title-less hints.
 * @property {String} [title]
 * @property {String} description
 * @property {String} [version]
 */

export const HINT_KIND = {
  plain: 'plain',
  function: 'function',
  module: 'module'
}

// Map specific hrefs to predefined hints (e.g. links to the build in types).
const predefinedHints = [{
  href: 'typespecs.html#basic-types',
  hint: {
    kind: HINT_KIND.plain,
    description: 'Basic type'
  }
}, {
  href: 'typespecs.html#literals',
  hint: {
    kind: HINT_KIND.plain,
    description: 'Literal'
  }
}, {
  href: 'typespecs.html#built-in-types',
  hint: {
    kind: HINT_KIND.plain,
    description: 'Built-in type'
  }
}]

const state = {
  // A function to cancel loading hint from the external documentation page.
  cancelHintFetching: null
}

/**
 * Checks if the given URL looks like something we may get a hint for.
 *
 * Ensures we're not displaying tooltips for non-html pages and module documentation sections.
 *
 * @param {String} href Link to a documentation page.
 * @returns {Boolean} Whether the link looks hint-able.
 */
export function isValidHintHref (href) {
  if (findPredefinedHint(href)) { return true }

  // Regex for hashes that will trigger the tooltip.
  // Allows us to avoid displaying tooltips for module sections (i.e. `Module.html#functions`).
  // Note: the regex is intended to check for the arity slash.
  const supportedHashRegex = /#.*\//
  const unsupportedHash = href.includes('#') && !supportedHashRegex.test(href)
  if (unsupportedHash) { return false }

  const validExtension = href.includes('.html')

  return validExtension
}

/**
 * Loads a hint object for the given documentation page.
 * Takes a predefined list of hints into account as well.
 *
 * @param {String} href Link to a documentation page.
 * @returns {Promise<Hint>} A promise resolving to the matching hint if found.
 */
export function getHint (href) {
  const predefinedHint = findPredefinedHint(href)

  if (predefinedHint) {
    return Promise.resolve(predefinedHint)
  }

  return loadHintFromExternalPage(href)
}

/**
 * Returns a predefined hint object for the given href or `null` if none matches.
 *
 * @param {String} href Link to a documentation page.
 * @returns {Hint|null} The matching hint.
 */
function findPredefinedHint (href) {
  const result = predefinedHints.find(predefinedHint => href.includes(predefinedHint.href))
  return result ? result.hint : null
}

/**
 * Loads the given documentation page and waits for a hint message to be sent.
 *
 * @param {String} href Link to a documentation page.
 * @returns {Promise<Hint>} A promise resolving to the matching hint if found.
 */
function loadHintFromExternalPage (href) {
  // Add the hint parameter, so that the target documentation page
  // knows it should send us a window message event.
  const hintHref = href.replace('.html', '.html?hint=true')

  return new Promise((resolve, reject) => {
    // Load the page in a hidden iframe, so that it may send a message to the window.
    const iframe = document.createElement('iframe')
    // The minimum permissions necessary for the iframe to run JavaScript and communicate with the parent window.
    iframe.setAttribute('sandbox', 'allow-scripts allow-same-origin')
    iframe.setAttribute('src', hintHref)
    iframe.style.display = 'none'

    function handleMessage (event) {
      const { href, hint } = event.data
      if (hintHref === href) {
        cleanup()
        resolve(hint)
      }
    }

    state.cancelHintFetching = () => {
      cleanup()
      reject(new Error('cancelled'))
    }

    function cleanup () {
      iframe.remove()
      window.removeEventListener('message', handleMessage)
      state.cancelHintFetching = null
    }

    window.addEventListener('message', handleMessage)

    document.body.appendChild(iframe)
  })
}

/**
 * Cancels hint request to an external documentation page if any is in progress.
 */
export function cancelHintFetchingIfAny () {
  if (state.cancelHintFetching) {
    state.cancelHintFetching()
  }
}

/**
 * Extracts info about a function, callback or a type defined inside a module.
 *
 * @param {HTMLElement} element A div containing the relevant data.
 * @returns {Hint}
 */
export function extractFunctionHint (element) {
  const heading = element.querySelector('h1')
  const title = heading.textContent

  const firstParagraph = element.querySelector('.docstring > p')
  const description = firstParagraph ? firstParagraph.innerHTML : ''

  return {
    kind: HINT_KIND.function,
    title: title.trim(),
    description: description.trim()
  }
}

/**
 * Extracts info about module.
 *
 * @param {HTMLElement} element A div containing the relevant data.
 * @returns {Hint}
 */
export function extractModuleHint (content) {
  const heading = content.querySelector('h1 > span')
  const title = heading.textContent

  const firstParagraph = content.querySelector('#moduledoc p')
  const description = firstParagraph ? firstParagraph.innerHTML : ''

  return {
    kind: HINT_KIND.module,
    title: title.trim(),
    description: description.trim()
  }
}
