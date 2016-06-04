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
  '#extras-list',
  '#modules-list',
  '#exceptions-list',
  '#protocols-list'
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
  $('#full-list > li.node:not(.clicked)').each(function () {
    $(this).addClass('collapsed').next('li.docs').addClass('collapsed')
  })

  // Scroll list to the selected one
  var $fullList = $('#full-list')
  var $clicked = $('#full-list .clicked')
  if ($clicked.length > 0) {
    $fullList.scrollTop(
      $clicked.offset().top - $fullList.offset().top - 40
    )
  }
}

/**
 * Fill the sidebar with links to different nodes
 *
 * This function replaces an empty unordered list with an
 * unordered list full of links to the different protocols, exceptions
 * and modules mentioned in the documentation.
 *
 * @param {Object} nodes - Container of protocols, exceptions and modules.
 * @param {String} filter - Filter of nodes, by default 'modules'.
 */
function fillSidebarWithNodes (nodes, filter) {
  function scope (items) {
    var filtered = nodes[items]
    var fullList = $('#full-list')
    fullList.replaceWith(sidebarItemsTemplate(filtered))
  }

  const module_type = helpers.getModuleType()

  filter = filter || module_type
  scope(filter)
  setupSelected(['#', filter, '-list'].join(''))

  var $docLinks = $('#full-list .doclink')
  var $docItems = $('#full-list .docs')
  var $defItems = $('#full-list .deflist > li')

  function handleAnchor (e) {
    e.preventDefault()

    var $target = $(e.target)

    $docItems.removeClass('active')
    $target.closest('li').addClass('active')

    var href = $target.attr('href')
    helpers.scrollTo(CONTENT, helpers.saveFind(href), function () {
      window.location.hash = href.replace(/^#/, '')
    })
  }

  $('#full-list .node.clicked > a').on('click', handleAnchor)
  $docLinks.on('click', handleAnchor)

  $('#full-list .node.clicked .deflink').on('click', e => {
    e.preventDefault()

    var $target = $(e.target)

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
  SIDEBAR_NAV.on('click', '#extras-list', createHandler('extras'))
  SIDEBAR_NAV.on('click', '#modules-list', createHandler('modules'))
  SIDEBAR_NAV.on('click', '#exceptions-list', createHandler('exceptions'))
  SIDEBAR_NAV.on('click', '#protocols-list', createHandler('protocols'))

  $('.sidebar-search input').on('keyup', function (e) {
    if (e.which === 13) { // enter key maps to 13
      search()
    } else if (e.which === 27) { // escape key
      $(this).val('')
    }
  })

  $('.sidebar-search i.icon-search').on('click', function (e) {
    search()
  })
}

function identifyCurrentHash () {
  var hash = window.location.hash

  if (!hash) return

  const nodes = sidebarNodes[helpers.getModuleType()]
  const category = helpers.findSidebarCategory(nodes, hash.replace(/^#/, ''))

  $(`#full-list .clicked a[href="#${category}"]`)
    .closest('li')
    .addClass('active')

  $(`#full-list .clicked a[href="${hash}"]`)
    .closest('li')
    .addClass('active')

  helpers.saveFind(hash)
}

function fixLinks () {
  CONTENT.find('a').has('code').addClass('no-underline')
  CONTENT.find('a').has('img').addClass('no-underline')
}

/**
 * Focus on the content element.
 *
 * This is required so that the space bar (and similar key bindings)
 * work as soon as you visit a module's documentation. Without this,
 * the user would be forced to first click on the content element
 * before these keybindings worked.
 */
function fixSpacebar () {
  CONTENT.attr('tabindex', -1).focus()
}

// Public Methods
// --------------

export function initialize () {
  fillSidebarWithNodes(sidebarNodes)
  addEventListeners()
  collapse()
  identifyCurrentHash()
  fixLinks()
  fixSpacebar()
}
