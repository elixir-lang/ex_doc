// Dependencies
// ------------

import $ from 'jquery'
import find from 'lodash.find'

// Constants
// ---------

const contentInner = '.content-inner'
const message = {summary: '', ready: false, requestId: null}
const typespecs = {
  pathnameEnd: '/typespecs.html',
  categories: [
    { name: 'basicType', hint: 'Basic type', hash: '#basic-types', detailsAvailable: false },
    { name: 'literal', hint: 'Literal', hash: '#literals', detailsAvailable: false },
    { name: 'builtInType', hint: 'Built-in type', hash: '#built-in-types', detailsAvailable: true }
  ]
}

function hashToElement (hash) {
  if (!hash) { return null }
  hash = hash.substr(1)

  if (!hash) { return null }
  hash = $.escapeSelector(hash)

  if (hash === '') { return null }

  return $(`#${hash}.detail`)
}

function typeCategory (hash) {
  return find(typespecs.categories, {hash: hash})
}

function focusFromHash () {
  const params = new URLSearchParams(window.location.search)
  const requestId = params.get('requestId')
  const typeName = params.get('typeName')
  let summary = null

  if (!params.has('focused')) { return }

  if (!requestId) { return }

  const infoElement = hashToElement(window.location.hash)

  if (infoElement && infoElement.length > 0) {
    summary = prepareFunctionSummary(infoElement)
  } else if (isTypesPage(params)) {
    summary = prepareTypeSummary(typeName)
  } else if (isModulePage()) {
    summary = preparePageSummary()
  }

  console.log("focus_mode - got summary", summary)

  if (!summary) { return }

  $(document).ready(function () {
    postMessage(summary, requestId)
  })
}

function postMessage (summary, requestId) {
  console.log('focus_mod - sending messages', summary)
  if (window.self !== window.parent) {
    message.summary = summary
    message.ready = true
    message.requestId = requestId
    window.parent.postMessage(message, '*')
  }
}

function prepareFunctionSummary (element) {
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

function preparePageSummary () {
  let content = $(contentInner)
  content.find('h1:first > *').remove()

  return {
    type: 'page',
    title: content.find('h1:first').text(),
    description: content.find('#moduledoc p:first').text().trim()
  }
}

function prepareTypeSummary (typeName) {
  const category = typeCategory(window.location.hash)
  const typeDetails = extractTypeDetails(category, typeName)

  if (!typeDetails) { return }
  if (!category) { return }

  return {
    type: 'type',
    typeCategory: category.name,
    title: typeDetails.title,
    description: typeDetails.description
  }
}

function extractTypeDetails (category, typeName) {
  const fullTypeName = `${typeName}()`

  if (category.detailsAvailable) {
    const detailsTable = $(contentInner).find(category.hash).nextAll('table').first()

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
      description: category.hint
    }
  }
}

function isModulePage () {
  return $(contentInner).find('#moduledoc').length > 0
}

function isTypesPage (params) {
  const isThisTypspecsPage = window.location.pathname.indexOf(typespecs.pathnameEnd) > 0
  const isTypesHashPresent = !!typeCategory(window.location.hash)
  const isTypeRequested = !!params.get('typeName')

  console.log("focus_mode - isTypesPage", isThisTypspecsPage, isTypesHashPresent, isTypeRequested)

  return isThisTypspecsPage && isTypesHashPresent && isTypeRequested
}

// Public Methods
// --------------

export function initialize () {
  focusFromHash()
}
