
import $ from 'jquery'

function extractFunctionSummary (element) {
  const signatureSpecs = element.find('h1 .specs').text()
  element.find('h1 > *').remove()
  const title = element.find('h1').text()
  const description = element.find('.docstring > p:first').text()

  return {
    type: 'function',
    title: title,
    signatureSpecs: signatureSpecs,
    description: description.trim()
  }
}

function extractModuleSummary (content) {
  content.find('h1:first > *').remove()

  return {
    type: 'page',
    title: content.find('h1:first').text(),
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
function extractTypeSummary (contentElement, typeName, typeCategory) {
  const typeDetails = extractTypeDetails(contentElement, typeCategory, typeName)

  if (!typeDetails) { return }
  if (!typeCategory) { return }

  return {
    type: 'type',
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
function extractTypeDetails (contentElement, category, typeName) {
  const fullTypeName = `${typeName}()`

  if (category.detailsAvailable) {
    const detailsTable = contentElement.find(category.hash).nextAll('table').first()

    if (detailsTable.length === 0) { return }

    console.log("focus_mode - details tale", detailsTable.text())

    const foundRow = detailsTable.find('tr').filter(function () {
      return $(this).find(`td:first:contains('${fullTypeName}')`).length > 0
    })

    console.log("focus_mode - foundRow", foundRow.text())

    let description = foundRow.find('td:last-child').text()

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

export {extractTypeSummary, extractModuleSummary, extractFunctionSummary}
