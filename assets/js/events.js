/* globals sidebarNodes */
'use strict'

// Dependencies
// ------------

var $ = require('jquery')
var search = require('./search')
var sidebarItemsTemplate = require('./sidebar-items.handlebars')

// Constants
// ---------

var SIDEBAR_TYPES = [
  '#modules_list',
  '#exceptions_list',
  '#protocols_list'
]
var SIDEBAR_NAV = $('.sidebar-mainNav')

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

  $('.sidebar-search input').on('keypress', function (e) {
    if (e.which === 13) { // enter key maps to 13
      search.start()
    }
  })
}

function initalize () {
  fillSidebarWithNodes(sidebarNodes)
  addEventListeners()
  collapse()
  identifyExternalLinks('#content')
}

module.exports = {
  initialize: initalize
}
