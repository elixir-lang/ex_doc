import { el } from './helpers'

const CONTENT_CONTAINER_ID = 'content'
const TABSET_OPEN_COMMENT = 'tabs-open'
const TABSET_CLOSE_COMMENT = 'tabs-close'
const TABPANEL_HEADING_NODENAME = 'H3'
const TABSET_CONTAINER_CLASS = 'tabset'

window.addEventListener('exdoc:loaded', initialize)
initialize()

function initialize () {
  /** @type {[Node, [NodeList, HTMLElement[]][]][]} */
  const sets = []
  /** @type {Node[]} */
  const toRemove = []

  const iterator = document.createNodeIterator(
    document.getElementById(CONTENT_CONTAINER_ID),
    NodeFilter.SHOW_COMMENT,
    (node) => node.nodeValue.trim() === TABSET_OPEN_COMMENT
      ? NodeFilter.FILTER_ACCEPT
      : NodeFilter.FILTER_REJECT
  )

  /** @type {Node} */
  let opener
  while ((opener = iterator.nextNode())) {
    const set = []
    sets.push([opener, set])

    /** @type {HTMLElement[]} */
    let tabContent

    let node = opener
    while ((node = node.nextSibling)) {
      if (node.nodeName === TABPANEL_HEADING_NODENAME) {
        // Tab heading.
        tabContent = []
        // Extract heading text nodes (faster than using .textContent which requires layout).
        const headingContent = node.querySelector('.text')?.childNodes || node.childNodes
        set.push([headingContent, tabContent])
        toRemove.push(node)
      } else if (node.nodeName === '#comment' && node.nodeValue.trim() === TABSET_CLOSE_COMMENT) {
        // Closer comment.
        toRemove.push(node)
        break
      } else if (tabContent) {
        // Tab content.
        tabContent.push(node)
      }
    }
  }

  sets.forEach(([opener, set], setIndex) => {
    const tabset = el('div', {
      class: TABSET_CONTAINER_CLASS
    })
    opener.parentNode.replaceChild(tabset, opener)

    const tablist = el('div', {
      role: 'tablist',
      class: 'tabset-tablist'
    })
    tabset.appendChild(tablist)

    set.forEach(([headingContent, content], index) => {
      const selected = index === 0
      const tabId = `tab-${setIndex}-${index}`
      const tabPanelId = `tabpanel-${setIndex}-${index}`

      const tab = el('button', {
        role: 'tab',
        id: tabId,
        class: 'tabset-tab',
        tabindex: selected ? 0 : -1,
        'aria-selected': selected,
        'aria-controls': tabPanelId
      }, headingContent)
      tab.addEventListener('click', handleTabClick)
      tab.addEventListener('keydown', handleTabKeydown)
      tablist.appendChild(tab)

      const tabPanel = el('div', {
        role: 'tabpanel',
        id: tabPanelId,
        class: 'tabset-panel',
        hidden: !selected ? '' : undefined,
        tabindex: selected ? 0 : -1,
        'aria-labelledby': tabId
      }, content)
      tabset.appendChild(tabPanel)
    })
  })

  toRemove.forEach((node) => {
    node.parentNode.removeChild(node)
  })
}

/** @param {MouseEvent} event */
function handleTabClick (event) {
  activateTab(event.currentTarget)
}

/** @param {KeyboardEvent} event */
function handleTabKeydown (event) {
  if (keys[event.code]) {
    event.preventDefault()
    const tabs = [...event.currentTarget.parentNode.childNodes]
    const currentIndex = tabs.indexOf(event.currentTarget)
    const newIndex = keys[event.code](currentIndex, tabs.length)
    activateTab(tabs.at(newIndex % tabs.length))
  }
}

/** @type {Dictionary<string, (index: number, length: number) => number>} */
const keys = {
  ArrowLeft: (index) => index - 1,
  ArrowRight: (index) => index + 1,
  Home: () => 0,
  End: (index, length) => length - 1
}

/** @param {HTMLButtonElement} tab */
function activateTab (tab) {
  const prev = tab.parentNode.querySelector('[aria-selected=true]')

  if (prev === tab) return

  // Set previously active tab button as inactive.
  prev.setAttribute('aria-selected', 'false')
  prev.tabIndex = -1

  // Set newly active tab button as active.
  tab.setAttribute('aria-selected', 'true')
  tab.tabIndex = 0
  tab.focus()

  // Set previously active tab panel as inactive.
  const prevPanel = document.getElementById(prev.getAttribute('aria-controls'))
  prevPanel.setAttribute('hidden', '')
  prevPanel.tabIndex = -1

  // Set newly active tab panel as active.
  const panel = document.getElementById(tab.getAttribute('aria-controls'))
  panel.removeAttribute('hidden')
  panel.tabIndex = 0
}
