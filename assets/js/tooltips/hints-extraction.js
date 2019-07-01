
// Dependencies
// ------------

/**
 * Extracts info about a function, callback or a type defined inside a module.
 *
 * @param {Object} element jQuery selector pointing to a div containing relevant data
 *
 * @returns {Object} hint info object
 */
function extractFunctionHint (element) {
  const signatureSpecs = element.find('.specs').text()
  const title = element.find('h1').text()
  const description = element.find('.docstring > p:first').text()

  return {
    kind: 'function',
    title: title.trim(),
    signatureSpecs: signatureSpecs.trim(),
    description: description.trim()
  }
}

function extractModuleHint (content) {
  content.find('h1:first > *').remove()

  return {
    kind: 'module',
    title: content.find('h1:first').text().trim(),
    description: content.find('#moduledoc p:first').text().trim()
  }
}

// Public Methods
// --------------

export {extractModuleHint, extractFunctionHint}
