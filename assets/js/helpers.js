/**
 * A shorthand for `document.querySelector`.
 * @type {Function}
 */
export const qs = document.querySelector.bind(document)

/**
 * A shorthand for `document.querySelectorAll`.
 * @type {Function}
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
  return document.body.dataset.type
}

/**
 * Looks up a nested node having the specified anchor
 * and returns the corresponding category.
 *
 * @param {Array} nodes A list of sidebar nodes.
 * @param {String|null} anchor The anchor to look for.
 * @returns {String} The relevant node group key, like 'functions', 'types', etc.
 */
export function findSidebarCategory (nodes, anchor) {
  if (!nodes) return

  for (const node of nodes) {
    const nodeGroup = node.nodeGroups && node.nodeGroups.find(nodeGroup =>
      nodeGroup.nodes.some(subnode => subnode.anchor === anchor)
    )

    if (nodeGroup) return nodeGroup.key
  }

  return null
}

/**
 * Returns current location hash without the leading hash character.
 *
 * @returns {String}
 */
export function getLocationHash () {
  return window.location.hash.replace(/^#/, '')
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
 * Return `true` if the client's OS is MacOS.
 *
 * @return {Boolean}
 */
export function isMacOS () {
  return /(Mac|iPhone|iPod|iPad)/i.test(navigator.platform)
}
