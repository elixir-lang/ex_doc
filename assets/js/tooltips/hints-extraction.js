
// Dependencies
// ------------

/**
 * Extracts info about a function, callback or a type defined inside a module.
 *
 * @param {Object} element a div containing the relevant data
 *
 * @returns {Object} hint info object
 */
function extractFunctionHint (element) {
  const heading = element.querySelector('h1')
  const title = heading.textContent

  const firstParagraph = element.querySelector('.docstring > p')
  const description = firstParagraph ? firstParagraph.textContent : ''

  return {
    kind: 'function',
    title: title.trim(),
    description: description.trim()
  }
}

function extractModuleHint (content) {
  const heading = content.querySelector('h1')
  while (heading.firstElementChild) {
    heading.removeChild(heading.firstElementChild)
  }
  const title = heading.textContent

  const firstParagraph = content.querySelector('#moduledoc p')
  const description = firstParagraph ? firstParagraph.textContent : ''

  return {
    kind: 'module',
    title: title.trim(),
    description: description.trim()
  }
}

// Public Methods
// --------------

export {extractModuleHint, extractFunctionHint}
