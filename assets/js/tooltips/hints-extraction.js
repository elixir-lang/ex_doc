
// Dependencies
// ------------

import $ from 'jquery'

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

/**
 * Extracts info about a type.
 *
 * @param {Object} contentElement jQuery selector targeting the documentation content
 * @param {Object} typeCategory category that the type belongs to
 * @param {string} typeName name of the requested type
 *
 * @returns {Object|null} hint info object or `null` if type info could not be found
 */
function extractTypeHint (contentElement, typeName, typeCategory) {
  const typeDetails = extractTypeDetails(contentElement, typeName, typeCategory)

  if (!typeDetails) { return }
  if (!typeCategory) { return }

  return {
    kind: 'type',
    typeCategory: typeCategory.name,
    title: typeDetails.title,
    description: typeDetails.description
  }
}

/**
 * Extracts type's title and description.
 * If this info cannot be found returns the basic category description (ie. `Basic type`).
 *
 * @param {Object} contentElement jQuery selector targeting the documentation content
 * @param {Object} category category that the type belongs to
 * @param {string} typeName name of the requested type
 *
 * @returns {Object|null} object containing the hint info or null if detailed info is not available
 */
function extractTypeDetails (contentElement, typeName, category) {
  const fullTypeName = `${typeName}()`

  if (category.detailsAvailable) {
    const detailsTable = contentElement.find(category.hash).nextAll('table').first()

    if (detailsTable.length === 0) { return }

    const foundRow = detailsTable.find('tr').filter(function () {
      return $(this).find('td:first').text().trim() === fullTypeName
    })

    let description = foundRow.find('td:last-child').text().trim()

    return {
      title: fullTypeName,
      description: description
    }
  } else {
    return {
      title: '',
      description: category.description
    }
  }
}

// Public Methods
// --------------

export {extractTypeHint, extractModuleHint, extractFunctionHint}
