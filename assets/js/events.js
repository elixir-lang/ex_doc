/* globals sidebarNodes */

// Dependencies
// ------------

import $ from 'jquery'
import {search} from './search'
import * as helpers from './helpers'

import sidebarItemsTemplate from './templates/sidebar-items.handlebars'

// Constants
// ---------

var SIDEBAR_TYPES = [
  '#extras-list',
  '#modules-list',
  '#exceptions-list',
  '#tasks-list',
  '#search-list'
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
  var $fullList = $('#full-list')
  var $currentPage = $('#full-list li.current-page')
  if ($currentPage.length > 0) {
    $fullList.scrollTop(
      $currentPage.offset().top - $fullList.offset().top - 40
    )
  }
}

/**
 * Fill the sidebar with links to different nodes
 *
 * This function replaces an empty unordered list with an
 * unordered list full of links to the different tasks, exceptions
 * and modules mentioned in the documentation.
 *
 * @param {Object} nodes - Container of tasks, exceptions and modules.
 * @param {String} filter - Filter of nodes, by default 'modules'.
 */
function fillSidebarWithNodes (nodes, filter) {
  const moduleType = helpers.getModuleType()

  filter = filter || moduleType
  var filtered = nodes[filter] || []
  var fullList = $('#full-list')
  fullList.replaceWith(sidebarItemsTemplate({'nodes': filtered, 'group': ''}))
  setupSelected(['#', filter, '-list'].join(''))

  $('#full-list li a').on('click', e => {
    var $target = $(e.target)
    // the user might have clicked on the nesting indicator
    var linkTag = $target.is('a') ? $target : $target.closest('a')
    if (linkTag.hasClass('expand') && !$target.is('span') && !e.shiftKey) {
      e.preventDefault()
      $(e.target).closest('li').toggleClass('open')
    } else {
      $('#full-list li.current-page li.current-hash').removeClass('current-hash')
      $(e.target).closest('li').addClass('current-hash')
    }
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
  SIDEBAR_NAV.on('click', '#tasks-list', createHandler('tasks'))

  $('.sidebar-search input').on('keydown', function (e) {
    if (e.keyCode === 27) { // escape key
      $(this).val('')
    } else if ((event.metaKey || event.ctrlKey) && e.keyCode === 13) { // cmd+enter
      $(this).parent().attr('target', '_blank').submit().removeAttr('')
      e.preventDefault()
    }
  })

  var pathname = window.location.pathname
  if (pathname.substr(pathname.lastIndexOf('/') + 1) === 'search.html') {
    search(getParameterByName('q'))
  }
}

function getParameterByName (name) {
  const url = window.location.href
  const param = name.replace(/[\[\]]/g, '\\$&')
  const regex = new RegExp('[?&]' + param + '(=([^&#]*)|&|#|$)')
  const results = regex.exec(url)
  if (!results) return ''
  if (!results[2]) return ''
  return decodeURIComponent(results[2].replace(/\+/g, ' '))
}

function identifyCurrentHash () {
  var hash = helpers.getLocationHash() || 'content'

  const nodes = sidebarNodes[helpers.getModuleType()] || []
  const category = helpers.findSidebarCategory(nodes, hash)

  $(`#full-list li.current-page a.expand[href$="#${category}"]`)
    .closest('li')
    .addClass('open')

  $(`#full-list li.current-page a[href$="#${hash}"]`)
    .closest('li')
    .addClass('current-hash')
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
