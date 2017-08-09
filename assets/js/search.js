/* globals sidebarNodes */

// Search
// ======

// Dependencies
// ------------

import $ from 'jquery'
import * as helpers from './helpers'
import resultsTemplate from './templates/search-results.handlebars'

// Local Variables
// ---------------

const $search = $('#search')
const $input = $('.sidebar-search input')

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

function findNested (elements, parentId, matcher) {
  return (elements || []).map(function (element) {
    // Match things like module.func
    var parentMatch = (parentId + '.' + element.id).match(matcher)
    var match = element.id && element.id.match(matcher)

    if (parentMatch || match) {
      var result = JSON.parse(JSON.stringify(element))
      result.match = match ? highlight(match) : element.id
      return result
    }
  }).filter(cleaner)
}

function pushLevel (levels, searchEntity, name) {
  if (searchEntity.length > 0) {
    levels.push({name: name, results: searchEntity})
  }
}

export function findIn (elements, matcher) {
  return elements.map(function (element) {
    var title = element.title
    var titleMatch = title && title.match(matcher)
    var functionMatches = findNested(element.functions, title, matcher)
    var macroMatches = findNested(element.macros, title, matcher)
    var callbackMatches = findNested(element.callbacks, title, matcher)
    var typeMatches = findNested(element.types, title, matcher)

    var result = {
      id: element.id,
      match: titleMatch ? highlight(titleMatch) : element.title
    }

    if (functionMatches.length > 0) result.functions = functionMatches
    if (macroMatches.length > 0) result.macros = macroMatches
    if (callbackMatches.length > 0) result.callbacks = callbackMatches
    if (typeMatches.length > 0) result.types = typeMatches

    if (titleMatch ||
        functionMatches.length > 0 ||
        macroMatches.length > 0 ||
        callbackMatches.length > 0 ||
        typeMatches.length > 0
       ) {
      return result
    }
  }).filter(cleaner)
}

export function search (value) {
  var nodes = sidebarNodes

  if (value.replace(/\s/, '') !== '') {
    var safeVal = new RegExp(helpers.escapeText(value), 'i')
    var levels = []

    var modules = findIn(nodes.modules, safeVal)
    var exceptions = findIn(nodes.exceptions, safeVal)
    var protocols = findIn(nodes.protocols, safeVal)
    var tasks = findIn(nodes.tasks, safeVal)

    // add to the results
    pushLevel(levels, modules, 'Modules')
    pushLevel(levels, exceptions, 'Exceptions')
    pushLevel(levels, protocols, 'Protocols')
    pushLevel(levels, tasks, 'Mix Tasks')

    var results = resultsTemplate({
      value: value,
      levels: levels,
      empty: levels.length === 0
    })

    $input.val(value)
    $search.html(results)
  }
}
