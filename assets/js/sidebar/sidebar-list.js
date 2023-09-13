import { qs, getCurrentPageSidebarType, getLocationHash, findSidebarCategory } from '../helpers'
import { getSidebarNodes } from '../globals'

const SIDEBAR_TYPE = {
  search: 'search',
  extras: 'extras',
  modules: 'modules',
  tasks: 'tasks'
}

const SIDEBAR_TAB_TYPES = [SIDEBAR_TYPE.extras, SIDEBAR_TYPE.modules, SIDEBAR_TYPE.tasks]

const sidebarNodeListSelector = type => `#${type}-full-list`

/**
 * Initializes the sidebar navigation list.
 */
export function initialize () {
  SIDEBAR_TAB_TYPES.forEach(type => {
    renderSidebarNodeList(getSidebarNodes(), type)
  })
  markActiveSidebarTab(getCurrentPageSidebarType())
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
  const nodeList = qs(sidebarNodeListSelector(type))
  const listContentHtml = Handlebars.templates['sidebar-items']({ nodes, group: '' })
  nodeList.innerHTML = listContentHtml

  // Removes the "expand" class from links belonging to single-level sections
  nodeList.querySelectorAll('ul').forEach(list => {
    if (list.innerHTML.trim() === '') {
      const emptyExpand = list.previousElementSibling
      if (emptyExpand.classList.contains('expand')) {
        emptyExpand.classList.remove('expand')
      }
      list.remove()
    }
  })

  // Register event listeners
  nodeList.querySelectorAll('li a + button').forEach(button => {
    button.addEventListener('click', event => {
      const target = event.target
      const listItem = target.closest('li')
      toggleListItem(listItem)
    })
  })

  nodeList.querySelectorAll('li a').forEach(anchor => {
    anchor.addEventListener('click', event => {
      const target = event.target
      const listItem = target.closest('li')
      const previousSection = nodeList.querySelector('.current-section')

      // Clear the previous current section
      if (previousSection) {
        clearCurrentSectionElement(previousSection)
      }

      if (anchor.matches('.expand') && anchor.pathname === window.location.pathname) {
        openListItem(listItem)
      }
    })
  })
}

function openListItem (listItem) {
  listItem.classList.add('open')
  listItem.querySelector('button[aria-controls]').setAttribute('aria-expanded', 'true')
}

function closeListItem (listItem) {
  listItem.classList.remove('open')
  listItem.querySelector('button[aria-controls]').setAttribute('aria-expanded', 'false')
}

function toggleListItem (listItem) {
  if (listItem.classList.contains('open')) {
    closeListItem(listItem)
  } else {
    openListItem(listItem)
  }
}

function markElementAsCurrentSection (section) {
  section.classList.add('current-section')
  section.querySelector('a').setAttribute('aria-current', 'true')
}

function clearCurrentSectionElement (section) {
  section.classList.remove('current-section')
  section.querySelector('a').setAttribute('aria-current', 'false')
}

function markElementAsCurrentHash (listItem) {
  listItem.classList.add('current-hash')
  listItem.querySelector('a').setAttribute('aria-current', 'true')
}

function clearCurrentHashElement (listItem) {
  listItem.classList.remove('current-hash')
  listItem.querySelector('a').setAttribute('aria-current', 'false')
}

function markActiveSidebarTab (activeType) {
  SIDEBAR_TAB_TYPES.forEach(type => {
    const button = qs(`#${type}-list-tab-button`)
    if (button) {
      const tabpanel = qs(`#${button.getAttribute('aria-controls')}`)
      if (type === activeType) {
        button.parentElement.classList.add('selected')
        button.setAttribute('aria-selected', 'true')
        button.setAttribute('tabindex', '0')
        tabpanel.removeAttribute('hidden')
      } else {
        button.parentElement.classList.remove('selected')
        button.setAttribute('aria-selected', 'false')
        button.setAttribute('tabindex', '-1')
        tabpanel.setAttribute('hidden', 'hidden')
      }
    }
  })
}

function scrollNodeListToCurrentCategory () {
  const nodeList = qs(sidebarNodeListSelector(getCurrentPageSidebarType()))
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
  const nodeList = qs(sidebarNodeListSelector(getCurrentPageSidebarType()))

  const categoryEl = nodeList.querySelector(`li.current-page a.expand[href$="#${category}"]`)
  if (categoryEl) {
    openListItem(categoryEl.closest('li'))
  }

  const hashEl = nodeList.querySelector(`li.current-page a[href$="#${hash}"]`)
  if (hashEl) {
    const deflist = hashEl.closest('ul')
    if (deflist.classList.contains('deflist')) {
      markElementAsCurrentSection(deflist.closest('li'))
    }
    markElementAsCurrentHash(hashEl.closest('li'))
  }
}

function addEventListeners () {
  // Bind the navigation links ("Pages", "Modules", "Tasks")
  // so that they render a list of all relevant nodes when clicked.
  SIDEBAR_TAB_TYPES.forEach(type => {
    const button = qs(`#${type}-list-tab-button`)
    if (button) {
      button.addEventListener('click', event => {
        markActiveSidebarTab(type)
        scrollNodeListToCurrentCategory()
      })
    }
  })

  // provide left/right arrow navigation for tablist, as required by ARIA authoring practices guide
  const tabList = qs('#sidebar-listNav')
  tabList.addEventListener('keydown', (e) => {
    if (e.key !== 'ArrowRight' && e.key !== 'ArrowLeft') { return }

    // SIDEBAR_TAB_TYPES cannot be used here as it always contains all possible types, not only types that the specific project has
    const tabTypes = Array.from(tabList.querySelectorAll('[role="tab"]')).map(tab => tab.dataset.type)
    // getCurrentPageSidebarType() cannot be used here as it's assigned once, on page render
    const currentTabType = tabList.querySelector('[role="tab"][aria-selected="true"]').dataset.type

    if (e.key === 'ArrowRight') {
      let nextTabTypeIndex = tabTypes.indexOf(currentTabType) + 1
      if (nextTabTypeIndex >= tabTypes.length) {
        nextTabTypeIndex = 0
      }

      const nextType = tabTypes[nextTabTypeIndex]
      markActiveSidebarTab(nextType)
      qs(`#${nextType}-list-tab-button`).focus()
    } else if (e.key === 'ArrowLeft') {
      let previousTabTypeIndex = tabTypes.indexOf(currentTabType) - 1
      if (previousTabTypeIndex < 0) {
        previousTabTypeIndex = tabTypes.length - 1
      }

      const previousType = tabTypes[previousTabTypeIndex]
      markActiveSidebarTab(previousType)
      qs(`#${previousType}-list-tab-button`).focus()
    }
  })

  // Keep .current-hash item in sync with the hash, regardless how the change takes place
  window.addEventListener('hashchange', event => {
    const nodeList = qs(sidebarNodeListSelector(getCurrentPageSidebarType()))
    const currentListItem = nodeList.querySelector('li.current-page li.current-hash')
    if (currentListItem) {
      clearCurrentHashElement(currentListItem)
    }

    markCurrentHashInSidebar()
  })
}
