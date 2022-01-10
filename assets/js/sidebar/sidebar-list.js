import sidebarItemsTemplate from '../handlebars/templates/sidebar-items.handlebars'
import { qs, getCurrentPageSidebarType, getLocationHash, findSidebarCategory } from '../helpers'
import { getSidebarNodes } from '../globals'

const SIDEBAR_TYPE = {
  search: 'search',
  extras: 'extras',
  modules: 'modules',
  tasks: 'tasks'
}

const SIDEBAR_NAV_TYPES = [SIDEBAR_TYPE.extras, SIDEBAR_TYPE.modules, SIDEBAR_TYPE.tasks]

const SIDEBAR_NODE_LIST_SELECTOR = '#full-list'

/**
 * Initializes the sidebar navigation list.
 */
export function initialize () {
  renderSidebarNodeList(getSidebarNodes(), getCurrentPageSidebarType())
  markCurrentHashInSidebar()
  scrollNodeListToCurrentCategory()
  addEventListeners()
}

/**
 * Fill the sidebar with links to different nodes
 *
 * This function replaces an empty unordered list with an
 * unordered list full of links to the different tasks, exceptions
 * and modules mentioned in the documentation.
 *
 * @param {Object} nodesByType - Container of tasks, exceptions and modules.
 * @param {String} type - Filter of nodes, by default the type of the current page.
 */
function renderSidebarNodeList (nodesByType, type) {
  const nodes = nodesByType[type] || []

  // Render the list
  const nodeList = qs(SIDEBAR_NODE_LIST_SELECTOR)
  const listContentHtml = sidebarItemsTemplate({ nodes: nodes, group: '' })
  nodeList.innerHTML = listContentHtml

  // Highlight the corresponding navigation link
  highlightNavigationLink(type)

  // Removes the "expand" class from links belonging to single-level sections
  nodeList.querySelectorAll('ul').forEach(list => {
    if (list.innerHTML.trim() === '') {
      const emptyExpand = list.previousElementSibling
      if (emptyExpand.classList.contains('expand')) {
        emptyExpand.classList.remove('expand')
      }
    }
  })

  // Register event listeners
  nodeList.querySelectorAll('li a').forEach(anchor => {
    anchor.addEventListener('click', event => {
      const target = event.target
      const listItem = target.closest('li')
      const previousSection = nodeList.querySelector('.current-section')

      // Expand icon should not navigate
      if (target.matches('.icon-expand')) {
        event.preventDefault()
        listItem.classList.toggle('open')
        return
      }

      // Clear the previous current section
      if (previousSection) {
        previousSection.classList.remove('current-section')
      }

      if (anchor.matches('.expand') && anchor.pathname === window.location.pathname) {
        listItem.classList.add('open')
      }
    })
  })
}

function highlightNavigationLink (activeType) {
  SIDEBAR_NAV_TYPES.forEach(type => {
    const anchor = qs(`#${type}-list-link`)
    if (anchor) {
      anchor.parentElement.classList.toggle('selected', type === activeType)
    }
  })
}

function scrollNodeListToCurrentCategory () {
  const nodeList = qs(SIDEBAR_NODE_LIST_SELECTOR)
  const currentPage = nodeList.querySelector('li.current-page')
  if (currentPage) {
    currentPage.scrollIntoView()
    nodeList.scrollTop -= 40
  }
}

function markCurrentHashInSidebar () {
  const hash = getLocationHash() || 'content'

  const sidebarNodes = getSidebarNodes()
  const nodes = sidebarNodes[getCurrentPageSidebarType()] || []
  const category = findSidebarCategory(nodes, hash)
  const nodeList = qs(SIDEBAR_NODE_LIST_SELECTOR)

  const categoryEl = nodeList.querySelector(`li.current-page a.expand[href$="#${category}"]`)
  if (categoryEl) {
    categoryEl.closest('li').classList.add('open')
  }

  const hashEl = nodeList.querySelector(`li.current-page a[href$="#${hash}"]`)
  if (hashEl) {
    const deflist = hashEl.closest('ul')
    if (deflist.classList.contains('deflist')) {
      deflist.closest('li').classList.add('current-section')
    }
    hashEl.closest('li').classList.add('current-hash')
  }
}

function addEventListeners () {
  // Bind the navigation links ("Pages", "Modules", "Tasks")
  // so that they render a list of all relevant nodes when clicked.
  SIDEBAR_NAV_TYPES.forEach(type => {
    const anchor = qs(`#${type}-list-link`)
    if (anchor) {
      anchor.addEventListener('click', event => {
        event.preventDefault()
        renderSidebarNodeList(getSidebarNodes(), type)
        scrollNodeListToCurrentCategory()
      })
    }
  })

  // Keep .current-hash item in sync with the hash, regardless how the change takes place
  window.addEventListener('hashchange', event => {
    const nodeList = qs(SIDEBAR_NODE_LIST_SELECTOR)
    const currentListItem = nodeList.querySelector('li.current-page li.current-hash')
    if (currentListItem) {
      currentListItem.classList.remove('current-hash')
    }

    markCurrentHashInSidebar()
  })
}
