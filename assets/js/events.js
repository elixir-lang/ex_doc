/* globals sidebarNodes */

// Dependencies
// ------------

import * as autocomplete from './autocomplete/display'
import {search} from './search'
import * as helpers from './helpers'
import {qs} from './helpers'

import sidebarItemsTemplate from './templates/sidebar-items.handlebars'

// Constants
// ---------

var SIDEBAR_TYPES = [
  '#extras-list',
  '#modules-list',
  '#tasks-list',
  '#search-list'
]
var SIDEBAR_NAV = qs('.sidebar-listNav')
var CONTENT = qs('.content')
var CONTENT_INNER = qs('.content-inner')
var BODY = qs('body')
var SEARCH_FORM = qs('form.sidebar-search')

function setupSelected (id) {
  SIDEBAR_TYPES.forEach(function (selector) {
    qs(selector).parentElement.classList.toggle('selected', selector === id)
  })
}

function collapse () {
  const fullList = qs('#full-list')
  const currentPage = qs('#full-list li.current-page')
  if (currentPage) {
    currentPage.scrollIntoView()
    fullList.scrollTop -= 40
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
  const filtered = nodes[filter] || []
  const fullList = qs('#full-list')
  const listContent = sidebarItemsTemplate({'nodes': filtered, 'group': ''})
  fullList.innerHTML = listContent
  setupSelected(['#', filter, '-list'].join(''))

  fullList.querySelectorAll('li a').forEach(anchor => {
    anchor.addEventListener('click', event => {
      const target = event.target
      if (anchor.classList.contains('expand') && !target.matches('span') && !event.shiftKey) {
        event.preventDefault()
        target.closest('li').classList.toggle('open')
      } else {
        const current = fullList.querySelector('li.current-page li.current-hash')
        if (current) {
          current.classList.remove('current-hash')
        }
        target.closest('li').classList.add('current-hash')
      }
    })
  })
}

function registerHandler (selector, name) {
  const anchor = SIDEBAR_NAV.querySelector(selector)
  if (anchor) {
    anchor.addEventListener('click', function (event) {
      event.preventDefault()
      fillSidebarWithNodes(sidebarNodes, name)
      collapse()
    })
  }
}

function addEventListeners () {
  registerHandler('#extras-list', 'extras')
  registerHandler('#modules-list', 'modules')
  registerHandler('#exceptions-list', 'exceptions')
  registerHandler('#tasks-list', 'tasks')

  qs('.sidebar-search .search-close-button').addEventListener('click', function (event) {
    closeSearch()
    event.preventDefault()
  })

  qs('.sidebar-search input').addEventListener('keydown', function (event) {
    var newWindowKeyDown = (event.metaKey || event.ctrlKey)
    var autocompleteSelection = autocomplete.selectedElement()

    if (event.keyCode === 27) { // escape key
      event.target.value = ''
      event.target.blur()
    } else if (event.keyCode === 13) { // enter
      if (autocompleteSelection) {
        var link = autocompleteSelection.getAttribute('href')
        handleAutocompleteEnterKey(event.target, newWindowKeyDown, link)
        event.preventDefault()
      } else if (newWindowKeyDown) {
        SEARCH_FORM.setAttribute('target', '_blank')
        SEARCH_FORM.submit()
        SEARCH_FORM.removeAttribute('target')
        event.preventDefault()
      }
    } else if (event.keyCode === 38) {
      autocomplete.moveSelection(-1)
      event.preventDefault()
    } else if (event.keyCode === 40) {
      autocomplete.moveSelection(1)
      event.preventDefault()
    } else if (!newWindowKeyDown) {
      autocomplete.update(event.target.value)
    }
  })

  qs('.sidebar-search input').addEventListener('keyup', function (event) {
    var commandKey = (event.metaKey || event.ctrlKey)
    if (commandKey) {
      return null
    }

    if (event.keyCode !== 38 && event.keyCode !== 40) { // Left and right arrow keys
      autocomplete.update(event.target.value)
    }
  })

  qs('.sidebar-search input').addEventListener('focus', function (event) {
    BODY.classList.add('search-focused')
    autocomplete.update(event.target.value)
  })

  qs('.sidebar-search input').addEventListener('blur', function (event) {
    const relatedTarget = event.relatedTarget

    if (relatedTarget) {
      if (relatedTarget.classList.contains('autocomplete-suggestion')) {
        return null
      }

      if (relatedTarget.classList.contains('search-close-button')) {
        closeSearch()
      }
    }

    BODY.classList.remove('search-focused')
    autocomplete.hide()
  })

  var pathname = window.location.pathname
  if (pathname.substr(pathname.lastIndexOf('/') + 1) === 'search.html') {
    search(getParameterByName('q'))
  }
}

function handleAutocompleteEnterKey (inputElement, newWindowKeyDown, link) {
  var target = newWindowKeyDown ? '_blank' : '_self'
  var originalValue = inputElement.value

  inputElement.removeAttribute('name')
  inputElement.value = ''

  SEARCH_FORM.setAttribute('action', link)
  SEARCH_FORM.setAttribute('target', target)
  SEARCH_FORM.submit()
  SEARCH_FORM.setAttribute('action', 'search.html')

  inputElement.value = originalValue
  inputElement.setAttribute('name', 'q')
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

  const categoryEl = qs(`#full-list li.current-page a.expand[href$="#${category}"]`)
  if (categoryEl) {
    categoryEl.closest('li').classList.add('open')
  }

  const hashEl = qs(`#full-list li.current-page a[href$="#${hash}"]`)
  if (hashEl) {
    hashEl.closest('li').classList.add('current-hash')
  }
}

function closeSearch () {
  const input = qs('.sidebar-search input')
  input.value = ''
  input.blur()
}

function fixLinks () {
  CONTENT.querySelectorAll('a').forEach(anchor => {
    if (anchor.querySelector('code, img')) {
      anchor.classList.add('no-underline')
    }
  })
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
  CONTENT_INNER.setAttribute('tabindex', -1)
  CONTENT_INNER.focus()
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
