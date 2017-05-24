/* globals sidebarNodes */

// Search
// ======

// Dependencies
// ------------

import $ from 'jquery'
import * as helpers from './helpers'
import {closeSidebar, breakpoint} from './sidebar'
import resultsTemplate from './templates/search-results.handlebars'

// Local Variables
// ---------------

const $content = $('.content-inner')
const $input = $('.sidebar-search input')
const $sidebarItems = $('#full-list li')
var $results

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

function search (nodes, value) {
  var safeVal = new RegExp(helpers.escapeText(value), 'i')

  var levels = []

  var modules = findIn(nodes.modules, safeVal)
  var exceptions = findIn(nodes.exceptions, safeVal)
  var protocols = findIn(nodes.protocols, safeVal)
  var tasks = findIn(nodes.tasks, safeVal)

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

  if (tasks.length > 0) {
    levels.push({
      name: 'Mix Tasks',
      results: tasks
    })
  }

  if ($results) {
    $results.remove()
  }

  $results = $(resultsTemplate({
    value: value,
    levels: levels,
    empty: levels.length === 0
  }))

  var $oldContent = $content.children()
  $oldContent.hide()
  $content.append($results)

  // Auto-hide Menu if on Mobile device
  window.screen.width < breakpoint ? closeSidebar() : null

  function closeResults (e) {
    var event = e || window.event
    var $hashElement = document.getElementById(helpers.getLocationHash())
    if (typeof event === 'object' && event !== null) {
      if (event.metaKey || event.shiftKey || event.altKey ||
          event.ctrlKey || event.button === 1 || event.button === 2) {
        return
      }
    }

    $results.remove()
    $oldContent.fadeIn()
    if ($hashElement && $hashElement.scrollIntoView) {
      $hashElement.scrollIntoView()
    }
  }

  $results.find('.close-search').on('click', function (e) {
    e.preventDefault()
  })

  $.merge($results.find('a'), $sidebarItems).on('click', closeResults)

  $results.fadeIn(function () {
    // Scroll the container with all elements
    $content.parent().scrollTop(0)
  })
}

// Public Methods
// --------------

export function start () {
  var searchVal = $input.val()

  if (searchVal === '') return

  search(sidebarNodes, searchVal)
}
