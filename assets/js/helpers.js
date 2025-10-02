/**
 * A shorthand for `document.querySelector`.
 * @type {typeof document.querySelector}
 */
export const qs = document.querySelector.bind(document)

/**
 * A shorthand for `document.querySelectorAll`.
 * @type {typeof document.querySelectorAll}
 */
export const qsAll = document.querySelectorAll.bind(document)

/**
 * Escapes all special characters in the given text,
 * so that it can be safely interpolated into a regular expression.
 *
 * @param {String} text Plain text.
 * @returns {String} The escaped text.
 */
export function escapeRegexModifiers (text) {
  return text.replace(/[\-\[\]{}()*+?.,\\\^$|#\s]/g, '\\$&')
}

/**
 * Escapes all special characters in the given text,
 * so that it can be safely interpolated into HTML.
 *
 * @param {String} text Plain text.
 * @returns {String} The escaped text.
 */
export function escapeHtmlEntities (text) {
  return String(text)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
}

/**
 * Retrieves the sidebar type corresponding to the
 * current page (like 'extras', 'modules', 'tasks', 'search').
 *
 * @returns {String}
 */
export function getCurrentPageSidebarType () {
  return document.getElementById('main').dataset.type
}

const headingTagNames = ['H1', 'H2', 'H3', 'H4', 'H5', 'H6']

/**
 * Finds an element by a URL hash (e.g. a function section).
 *
 * @param {Boolean} anything Whether or not to support any link to any header.
 * @returns {HTMLElement|null} The relevant element.
 */
export function descriptionElementFromHash (anything = false) {
  const hash = window.location.hash.replace(/^#/, '')

  if (!hash) {
    if (!anything) {
      return null
    } else {
      return document.getElementById('top-content')
    }
  }

  const element = document.getElementById(hash)
  if (!element) { return null }

  // See `detail_template.eex` for the reference.
  if (element.matches('.detail')) {
    return element
  }

  // Matches a subheader in particular
  if (headingTagNames.includes(element.tagName)) {
    return headingPreview(element)
  }

  const nearestHeading = findNearestAboveHeading(element)

  if (nearestHeading) {
    return nearestHeading
  } else {
    return document.getElementById('top-content')
  }
}

function findNearestAboveHeading (element) {
  let previous = element.previousElementSibling
  while (previous) {
    if (headingTagNames.includes(previous.tagName)) {
      return headingPreview(previous)
    }
    previous = previous.previousElementSibling
  }
  let parent = element.parentNode
  while (parent) {
    previous = parent.previousElementSibling
    while (previous) {
      if (headingTagNames.includes(previous.tagName)) {
        return headingPreview(previous)
      }
      previous = previous.previousElementSibling
    }
    parent = parent.parentNode
  }
  return null
}

function headingPreview (element) {
  const div = document.createElement('div')
  const nodes = [element]

  // Capture all nodes under the current heading level.
  let node = element
  while ((node = node.nextSibling)) {
    if (headingTagNames.includes(node.tagName) && node.tagName <= element.tagName) {
      break
    } else {
      nodes.push(node)
    }
  }

  div.append(...nodes)
  return div
}

/**
 * Retrieves decoded query parameter with the given name from current location.
 *
 * @param {String} name Name of the parameter to retrieve.
 * @returns {String|null}
 */
export function getQueryParamByName (name) {
  const params = new URLSearchParams(window.location.search)
  return params.get(name)
}

/**
 * Makes a GET request to the given URL and resolves
 * to `true` if it is successful.
 *
 * Note: cross-origin requests fail unless there are
 * proper CORS headers returned, because there is no
 * way to read the status of such response.
 *
 * @param {String} url The URL to check.
 * @returns {Promise} A promise resolving to `true` if the request succeeded
 *                    and `false` otherwise.
 */
export function checkUrlExists (url) {
  return fetch(url)
    .then(response => response.ok)
    .catch(() => false)
}

/**
 * Runs the given callback as soon as the whole document is loaded
 * and DOM is ready for manipulation.
 *
 * @param {Function} callback
 */
export function onDocumentReady (callback) {
  if (document.readyState !== 'loading') {
    callback()
  } else {
    document.addEventListener('DOMContentLoaded', callback)
  }
}

/**
 * Checks if the given text consists of white characters only.
 *
 * @param {String} text
 * @returns {Boolean}
 */
export function isBlank (text) {
  return !text || text.trim() === ''
}

/**
 * Returns a debounced function, so that the actual execution
 * is triggered only when there are no more calls
 * within the specified number of milliseconds.
 *
 * @param {Function} fn
 * @param {Number} milliseconds
 */
export function debounce (fn, milliseconds) {
  let timeout

  return function debouncedFunction (...args) {
    clearTimeout(timeout)

    timeout = setTimeout(() => {
      timeout = null
      fn(...args)
    }, milliseconds)
  }
}

/**
 * Grabs project version name from the meta tag.
 *
 * @returns {String} Project version name (e.g. "Elixir v1.2.3").
 */
export function getProjectNameAndVersion () {
  return document.head.querySelector('meta[name=project][content]').content
}

/**
 * Return `true` if the client's OS is Apple.
 *
 * @return {Boolean}
 */
export function isAppleOS () {
  // Set in inline_html.js
  return document.documentElement.classList.contains('apple-os')
}

/**
 * Create element from tag, attributes and children.
 *
 * @param {string} tagName
 * @param {Record<string, any>} attributes
 * @param {(HTMLElement | string)[]} [children]
 * @returns {HTMLElement}
 */
export function el (tagName, attributes, children) {
  const element = document.createElement(tagName)
  for (const key in attributes) {
    if (attributes[key] != null) {
      element.setAttribute(key, attributes[key])
    }
  }
  if (children) {
    element.replaceChildren(...children)
  }
  return element
}
