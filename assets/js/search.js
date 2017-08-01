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
let $results
let $oldContent
let searchCount = 0

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

export function getParameterByName (name, url) {
  if (!url) {
    url = window.location.href
  }
  const param = name.replace(/[\[\]]/g, '\\$&')
  const regex = new RegExp('[?&]' + param + '(=([^&#]*)|&|#|$)')
  const results = regex.exec(url)
  if (!results) {
    return null
  }
  if (!results[2]) {
    return ''
  }
  return decodeURIComponent(results[2].replace(/\+/g, ' '))
}

function pushLevel (levels, searchEntity, name) {
  if (searchEntity.length > 0) {
    levels.push({name: name, results: searchEntity})
  }
}

function search (nodes, value, addHistory) {
  var safeVal = new RegExp(helpers.escapeText(value), 'i')

  var levels = []

  var modules = findIn(nodes.modules, safeVal)
  var exceptions = findIn(nodes.exceptions, safeVal)
  var protocols = findIn(nodes.protocols, safeVal)
  var tasks = findIn(nodes.tasks, safeVal)

  // sometimes we need to stop adding to the history if it already exists.
  if (addHistory !== false && location.protocol !== 'file:') {
    // we use this to track searches that are in the history
    searchCount++
    history.pushState({searchValue: value}, 'Searching for ' + value, 'search.html?q=' + value)
  }

  // add to the results
  pushLevel(levels, modules, 'Modules')
  pushLevel(levels, exceptions, 'Exceptions')
  pushLevel(levels, protocols, 'Protocols')
  pushLevel(levels, tasks, 'Mix Tasks')

  if ($results) {
    $results.remove()
  }

  $results = $(resultsTemplate({
    value: value,
    levels: levels,
    empty: levels.length === 0
  }))

  $oldContent = $content.children()
  $oldContent.hide()
  $content.append($results)

  // Auto-hide Menu if on Mobile device
  window.screen.width < breakpoint ? closeSidebar() : null

  // we use history to close the search
  $results.find('.close-search').on('click', function (e) {
    e.preventDefault()
    closeResults(e)
    history.go(-searchCount)
  })

  // every other link closes the search
  $.merge($results.find('a').not('.close-search'), $sidebarItems).on('click', function (e) {
    closeResults(e, false)
  })

  $results.fadeIn(function () {
    // Scroll the container with all elements
    $content.parent().scrollTop(0)
  })
}

function closeResults (e, removeResults) {
  var event = e || window.event
  var $hashElement = document.getElementById(helpers.getLocationHash())
  if (typeof event === 'object' && event !== null) {
    if (event.metaKey || event.shiftKey || event.altKey ||
        event.ctrlKey || event.button === 1 || event.button === 2) {
      return
    }
  }
  if ($hashElement && $hashElement.scrollIntoView) {
    $hashElement.scrollIntoView()
  }

  if (removeResults !== false) {
    // clear the search bar if someone closes results.
    $input.val('')
    if ($results) {
      $results.remove()
    }
    if ($oldContent) {
      $oldContent.fadeIn()
    }
  }
}

// Public Methods
// --------------

export function start (val, addHistory) {
  var searchVal = val || $input.val()
  if (searchVal === '') return
  search(sidebarNodes, searchVal, addHistory)
}

export function popstateHandler (event) {
  if (searchCount > 0) {
    searchCount--
  }

  if (event.originalEvent.state == null) {
    // NOTE: for reasons only known to the browser makers we need to reload here,
    //       on back after navigating away the page (clicking a result in the search)
    //       there is no original page content i.e. all that was display none is gone
    //       note this doesn't happen in Safari, just FF and Chrome.
    // document.location.reload(true)
  } else if ('searchValue' in event.originalEvent.state) {
    // when we have a searchValue, show the search but clearly don't push a history state
    var searchValue = event.originalEvent.state.searchValue
    $input.val(searchValue) // set the search box to the searchValue for this state
    search(sidebarNodes, searchValue, false) // no history here, we already have one
  }
}
