/* globals sidebarNodes */
'use strict'

// Search
// ======

// Dependencies
// ------------

var $ = require('jquery')
var helpers = require('./helpers')
var resultsTemplate = require('./results-template.handlebars')

// Local Variables
// ---------------

var $content = $('.content-inner')
var $input = $('.sidebar-search input')

// Local Methods
// -------------

function highlight (match) {
  var start = match.index
  var end = match.index + match[0].length
  var input = match.input
  var highlighted = '<em>' + match[0] + '</em>'

  return input.slice(0, start) + highlighted + input.slice(end)
}

function cleaner (element) {
  return !!element
}

function findNested (elements, matcher) {
  return (elements || []).map(function (element) {
    var match = element.id && element.id.match(matcher)

    if (match) {
      var result = JSON.parse(JSON.stringify(element))
      result.match = highlight(match)
      return result
    }
  }).filter(cleaner)
}

function findIn (elements, matcher) {
  return elements.map(function (element) {
    var idMatch = element.id && element.id.match(matcher)
    var functionMatches = findNested(element.functions, matcher)
    var macroMatches = findNested(element.macros, matcher)
    var callbackMatches = findNested(element.callbacks, matcher)

    var result = {
      id: element.id,
      match: idMatch ? highlight(idMatch) : element.id
    }

    if (functionMatches.length > 0) result.functions = functionMatches
    if (macroMatches.length > 0) result.macros = macroMatches
    if (callbackMatches.length > 0) result.callbacks = callbackMatches

    if (idMatch ||
        functionMatches.length > 0 ||
        macroMatches.length > 0 ||
        callbackMatches.length > 0
       ) {
      return result
    }
  }).filter(cleaner)
}

function search (nodes, value) {
  var safeVal = new RegExp(helpers.escapeText(value), 'i')

  var levels = []

  var modules = findIn(nodes.modules, safeVal)
  var exceptions = findIn(nodes.exceptions, safeVal)
  var protocols = findIn(nodes.protocols, safeVal)

  if (modules.length > 0) {
    levels.push({
      name: 'Modules',
      results: modules
    })
  }

  if (exceptions.length > 0) {
    levels.push({
      name: 'Exceptions',
      results: exceptions
    })
  }

  if (protocols.length > 0) {
    levels.push({
      name: 'Protocols',
      results: protocols
    })
  }

  $content.hide()
  $content.html(resultsTemplate({
    value: value,
    levels: levels
  }))
  $content.fadeIn()
}

// Public Methods
// --------------

function start () {
  var searchVal = $input.val()

  if (searchVal === '') return

  search(sidebarNodes, searchVal)
}

module.exports = {
  start: start,
  findIn: findIn
}
