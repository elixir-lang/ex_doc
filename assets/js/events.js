/* globals sidebarNodes */

// Dependencies
// ------------

import $ from 'jquery'
import {start as search} from './search'
import * as helpers from './helpers'

import sidebarItemsTemplate from './templates/sidebar-items.handlebars'

// Constants
// ---------

var SIDEBAR_TYPES = [
  '#extras_list',
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
  function scope (items) {
    var filtered = nodes[items]
    var fullList = $('#full_list')
    fullList.replaceWith(sidebarItemsTemplate(filtered))
  }

  const module_type = helpers.getModuleType()

  filter = filter || module_type
  scope(filter)
  setupSelected(['#', filter, '_list'].join(''))

  var $docLinks = $('#full_list .doclink')
  var $docItems = $('#full_list .docs')
  var $defItems = $('#full_list .deflist > li')

  function handleAnchor (e) {
    e.preventDefault()

    var $target = $(event.target)

    $docItems.removeClass('active')
    $target.closest('li').addClass('active')

    var href = $target.attr('href')
    helpers.scrollTo(CONTENT, helpers.saveFind(href), function () {
      window.location.hash = href.replace(/^#/, '')
    })
  }

  $('#full_list .node.clicked > a').on('click', handleAnchor)
  $docLinks.on('click', handleAnchor)

  $('#full_list .node.clicked .deflink').on('click', e => {
    e.preventDefault()

    var $target = $(event.target)

    $defItems.removeClass('active')
    $target.closest('li').addClass('active')

    var href = $target.attr('href')
    helpers.scrollTo(CONTENT, helpers.saveFind(href), function () {
      window.location.hash = href.replace(/^#/, '')
    })
  })
}

function createHandler (name) {
  return function (event) {
    event.preventDefault()
    fillSidebarWithNodes(sidebarNodes, name)
    collapse()
  }
}

function addEventListeners () {
  SIDEBAR_NAV.on('click', '#extras_list', createHandler('extras'))
  SIDEBAR_NAV.on('click', '#modules_list', createHandler('modules'))
  SIDEBAR_NAV.on('click', '#exceptions_list', createHandler('exceptions'))
  SIDEBAR_NAV.on('click', '#protocols_list', createHandler('protocols'))

  $('.sidebar-search input').on('keyup', function (e) {
    if (e.which === 13) { // enter key maps to 13
      search()
    } else if (e.which === 27) { // escape key
      $(this).val('')
    }
  })

  $('.sidebar-search i.fa-search').on('click', function (e) {
    search()
  })
}

function identifyCurrentHash () {
  var hash = window.location.hash

  if (!hash) return

  const nodes = sidebarNodes[helpers.getModuleType()]
  const category = helpers.findSidebarCategory(nodes, hash.replace(/^#/, ''))

  $(`#full_list .clicked a[href="#${category}_details"]`)
    .closest('li')
    .addClass('active')

  $(`#full_list .clicked a[href="${hash}"]`)
    .closest('li')
    .addClass('active')

  helpers.scrollTo(CONTENT, helpers.saveFind(hash))
}

// Public Methods
// --------------

export function initialize () {
  fillSidebarWithNodes(sidebarNodes)
  addEventListeners()
  collapse()
  identifyCurrentHash()
}
