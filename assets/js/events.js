/* globals sidebarNodes */
'use strict'

// Dependencies
// ------------

var $ = require('jquery')
var helpers = require('./helpers')
var sidebarItemsTemplate = require('./sidebar-items.handlebars')

// Local Variables
// ---------------

var inSearch = null
var defaultSearchItemTimeOut = 0 // set to "0" if not testing
var searchIndex = 0
var searchCache = []
var searchString = ''
var regexSearchString = ''
var caseSensitiveMatch = false
var sidebarNav = $('.nav')

// Local Methods
// -------------

function highlight (no_padding) {
  var n = 1
  $('#full_list a.object-link:visible').each(function () {
    var next = n === 1 ? 2 : 1
    var li = $(this).parent()

    li.removeClass('r' + next).addClass('r' + n)
    if (no_padding) {
      li.addClass('no-padding')
    } else {
      li.removeClass('no-padding')
    }
    n = next
  })
}

/**
 * When the search field is empty show the children nodes of the #full_list
 *
 * Also removes the class .search-uncollapsed, .in-search, .found and .loading
 * among other things to reset the sidebar default status
 */
function showAllResults () {
  clearTimeout(inSearch)
  inSearch = defaultSearchItemTimeOut
  $('.search-uncollapsed').removeClass('search-uncollapsed')
  $('#sidebar').removeClass('in-search')
  $('#full_list li').removeClass('found').each(function () {
    var link = $(this).find('a.object-link:first')
    link.text(link.text())
  })
  $('#no_results').text('')
  $('#spinning span').removeClass('fa fa-refresh fa-spin')
  $('#search button span.fa-times').addClass('fa-search').removeClass('fa-times')
  highlight()
}

/**
 * If no results were found, shows a message to the user.
 *
 * Also remove the 'loading' icon and clear the timeout returned
 * initially by setTimeout.
 */
function searchDone () {
  highlight(true)
  if ($('#full_list li.found').size() === 0) {
    $('#no_results').text('No results were found.').hide().fadeIn()
  } else {
    $('#no_results').text('')
  }

  $('#spinning span').removeClass('fa fa-refresh fa-spin')
  clearTimeout(inSearch)
  inSearch = null
}

function searchItem () {
  var i
  var item
  var searchName
  var matchString
  var matchRegexp

  for (i = 0; i < searchCache.length / 50; i += 1) {
    item = searchCache[searchIndex]
    searchName = (searchString.indexOf('.') !== -1 ? item.fullName : item.name)
    matchString = regexSearchString
    matchRegexp = new RegExp(matchString, caseSensitiveMatch ? '' : 'i')

    if (searchName.match(matchRegexp) === null) {
      item.node.removeClass('found')
    } else {
      item.node.addClass('found')
      item.node.parents('li').addClass('search-uncollapsed')
      item.link.html(item.name.replace(matchRegexp, '<strong>$&</strong>'))
    }

    if (searchCache.length === searchIndex + 1) {
      searchDone()
      return
    }
    searchIndex += 1
  }
  inSearch = setTimeout(function () {
    searchItem()
  }, defaultSearchItemTimeOut)
}

/**
 * Fill the searchCache Array with the links found inside of the #full_list
 *
 * Define the initial set of events for the search field.
 */
function fullListSearch () {
  // generate cache
  searchCache = []
  $('#spinning span').removeClass('fa fa-refresh fa-spin')

  $('#full_list li').each(function () {
    var link = $(this).find('a.object-link:first')
    var fullName

    if (link.attr('title')) {
      fullName = link.attr('title').split(' ')[0]
      searchCache.push({
        name: link.text(),
        fullName: fullName,
        node: $(this),
        link: link
      })
    }
  })

  $('#search input').focus()
}

function performSearch () {
  searchString = $('#search input').val()
  caseSensitiveMatch = searchString.match(/[A-Z]/) !== null
  regexSearchString = helpers.escapeText(searchString)

  if (searchString === '') {
    showAllResults()
  } else {
    if (inSearch) {
      clearTimeout(inSearch)
    }
    $('#spinning span').addClass('fa fa-refresh fa-spin')
    $('#search button span.fa-search').addClass('fa-times').removeClass('fa-search')
    searchIndex = 0
    $('#sidebar').addClass('in-search')
    $('#no_results').text('')
    searchItem()
  }
}

/**
 * Identify external links inside of an specific section
 *
 * This function adds an icon to identify an external link.
 *
 * @param {String} section  Section where we want to identify the external links.
 */
function identifyExternalLinks (section) {
  $([section, 'a'].join(' ')).filter(function () {
    return (this.hostname !== window.location.hostname && $(this).attr('rel') !== 'help')
  }).append($('<span/>').attr({
    'class': 'fa fa-external-link',
    'aria-hidden': 'true'
  })).addClass('external')
}

function setupSelected (id) {
  ['#modules_list', '#exceptions_list', '#protocols_list'].forEach(function (element) {
    if (element === id) {
      $(element).parent().addClass('selected')
    } else {
      $(element).parent().removeClass('selected')
    }
  })
}
function collapse () {
  $('#full_list > li.node:not(.clicked)').each(function () {
    $(this).addClass('collapsed').next('li.docs').addClass('collapsed')
  })

  highlight()
}

function resetSidebar () {
  fullListSearch()
  collapse()
}

// Public Methods
// --------------

/**
 * Fill the sidebar with links to different nodes
 *
 * This function replace an empty unordered list with an
 * an unordered list full of links to the different procotols, exceptions
 * and modules mentioned in the documentation.
 *
 * @param {Object} nodes - Container of protocols, exceptions and modules.
 * @param {String} filter - Filter of nodes, by default 'modules'.
 */
function fillSidebarWithNodes (nodes, filter) {
  var module_type

  function scope (items) {
    var filtered = nodes[items]
    var fullList = $('#full_list')
    fullList.replaceWith(sidebarItemsTemplate(filtered))
  }

  module_type = $('#content h1 small').text()
  if (module_type && (module_type === 'exception' || module_type === 'protocol')) {
    module_type = module_type + 's' // pluralize 'exception' or 'protocol'
  } else {
    module_type = 'modules'
  }

  filter = filter || module_type
  scope(filter)
  setupSelected(['#', filter, '_list'].join(''))
}

function initalize () {
  sidebarNav.on('click', '#modules_list', function (e) {
    fillSidebarWithNodes(sidebarNodes, 'modules')
    resetSidebar()
    performSearch()
    e.preventDefault()
  })

  sidebarNav.on('click', '#exceptions_list', function (e) {
    fillSidebarWithNodes(sidebarNodes, 'exceptions')
    resetSidebar()
    performSearch()
    e.preventDefault()
  })

  sidebarNav.on('click', '#protocols_list', function (e) {
    fillSidebarWithNodes(sidebarNodes, 'protocols')
    resetSidebar()
    performSearch()
    e.preventDefault()
  })

  $('[data-toggle="offcanvas"]').on('click', function () {
    $('.row-offcanvas').toggleClass('active')
    $('#content').toggleClass('offcanvas-active')
  })

  $('#search button').on('click', function () {
    $('#search input').val('').focus()
    $('#search button span.fa-times').addClass('fa-search').removeClass('fa-times')
    showAllResults()
  })

  $(document).on('keyup', function (e) {
    var searchInput = $('#search input')
    if (e.keyCode === 27 && searchInput.val() !== '') { // escape key maps to 27
      searchInput.val('').focus()
      showAllResults()
    }
  })

  $('#search input').on('keypress', function (e) {
    if (e.which === 13) { // enter key maps to 13
      var firstLinkFound = document.querySelectorAll('#full_list li.found a.object-link')[0]
      firstLinkFound.click()
    }
  })

  $('#search input').on('input', function () {
    performSearch()
  })

  fullListSearch()
  collapse()
  identifyExternalLinks('#content')
}

// Set as global, so it's accessible in
// lib/ex_doc/formatter/html/templates/sidebar_items_template.eex
window.fillSidebarWithNodes = fillSidebarWithNodes

module.exports = {
  initialize: initalize
}
