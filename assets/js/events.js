/* globals sidebarNodes */
'use strict'

// Dependencies
// ------------

var $ = require('jquery')
var search = require('./search')
var helpers = require('./helpers')
var sidebarItemsTemplate = require('./sidebar-items.handlebars')

// Constants
// ---------

var SIDEBAR_TYPES = [
  '#modules_list',
  '#exceptions_list',
  '#protocols_list'
]
var SIDEBAR_NAV = $('.sidebar-listNav')
var CONTENT = $('.content')

function setupSelected (id) {
  SIDEBAR_TYPES.forEach(function (element) {
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

  // Scroll list to the selected one
  var $fullList = $('#full_list')
  var $clicked = $('#full_list .clicked')
  if ($clicked.length > 0) {
    $fullList.scrollTop(
      $clicked.offset().top - $fullList.offset().top - 40
    )
  }
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

  module_type = $('.content h1 small').text()
  if (module_type && (module_type === 'exception' || module_type === 'protocol')) {
    module_type = module_type + 's' // pluralize 'exception' or 'protocol'
  } else {
    module_type = 'modules'
  }

  filter = filter || module_type
  scope(filter)
  setupSelected(['#', filter, '_list'].join(''))

  var $docLinks = $('#full_list .docs a')
  var $docItems = $('#full_list .docs')

  function handleAnchor (e) {
    e.preventDefault()

    var $target = $(event.target)

    $docItems.removeClass('active')
    $target.closest('li').addClass('active')

    var href = $target.attr('href')
    helpers.scrollTo(CONTENT, $.find(href), function () {
      window.location.hash = href.replace(/^#/, '')
    })
  }

  $('#full_list .node.clicked > a').on('click', handleAnchor)
  $docLinks.on('click', handleAnchor)
}

function createHandler (name) {
  return function (event) {
    event.preventDefault()
    fillSidebarWithNodes(sidebarNodes, name)
    collapse()
  }
}

function addEventListeners () {
  SIDEBAR_NAV.on('click', '#modules_list', createHandler('modules'))
  SIDEBAR_NAV.on('click', '#exceptions_list', createHandler('exceptions'))
  SIDEBAR_NAV.on('click', '#protocols_list', createHandler('protocols'))

  $('.sidebar-search input').on('keyup', function (e) {
    if (e.which === 13) { // enter key maps to 13
      search.start()
    } else if (e.which === 27) { // escape key
      $(this).val('')
    }
  })

  $('.sidebar-search i.fa-search').on('click', function (e) {
    search.start()
  })
}

function identifyCurrentHash () {
  var hash = window.location.hash

  if (!hash) return

  $('#full_list .clicked a[href="' + hash + '"]')
    .closest('li')
    .addClass('active')

  helpers.scrollTo(CONTENT, $.find(hash))
}

function initalize () {
  fillSidebarWithNodes(sidebarNodes)
  addEventListeners()
  collapse()
  identifyCurrentHash()
}

module.exports = {
  initialize: initalize
}
