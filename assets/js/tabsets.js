const CONTENT_CONTAINER_ID = 'content'
const TABSET_OPEN_COMMENT = 'tabs-open'
const TABSET_CLOSE_COMMENT = 'tabs-close'
const TABPANEL_HEADING_NODENAME = 'H3'
const TABSET_CONTAINER_CLASS = 'tabset'

export function initialize () {
  const tabSetOpeners = getTabSetOpeners()
  const tabsetContainers = tabSetOpeners.map(processTabset)
  tabsetContainers.forEach(set => activateTabset(set))
}

function getTabSetOpeners () {
  const tabSetOpenersIterator = document.createNodeIterator(
    document.getElementById(CONTENT_CONTAINER_ID),
    NodeFilter.SHOW_COMMENT,
    {
      acceptNode (node) {
        return node.nodeValue.trim() === TABSET_OPEN_COMMENT
          ? NodeFilter.FILTER_ACCEPT
          : NodeFilter.FILTER_REJECT
      }
    }
  )

  const tabSetOpeners = []
  let opener
  while ((opener = tabSetOpenersIterator.nextNode())) {
    tabSetOpeners.push(opener)
  }

  return tabSetOpeners
}

/**
 * Mapped to an array of tabset opener comment markers, processes a tabset.
 * Prepares data held in HTML for the template, wraps the tabset elements,
 * and applies the template.
 *
 * @param {Element} element A tabset opener comment node.
 * @param {Integer} tabSetIndex
 * @param {Array} array
 * @returns {Array} Tabset container element.
 */
function processTabset (element, tabSetIndex, _array) {
  const allSetNodes = []
  const tabSet = []
  const tabPanel = {
    label: '',
    content: []
  }

  while ((element = element.nextSibling)) {
    if (isTabSetCloser(element)) {
      // Next node is closer comment; push current panel data and break.
      pushPanel(tabPanel, tabSet, tabSetIndex)
      break
    }

    // Push node to array of all tabset nodes, which are to be wrapped.
    allSetNodes.push(element)

    if (element.nodeName === TABPANEL_HEADING_NODENAME) {
      // Next node is tab heading; push current panel data, set next tab panel
      // heading text and reset next tab panel content array.
      pushPanel(tabPanel, tabSet, tabSetIndex)
      tabPanel.label = element.innerText
      tabPanel.content = []
    } else {
      // Next node is some other node; push to current tab panel content array.
      tabPanel.content.push(element.outerHTML)
    }
  }

  // Wrap all tabset nodes in new container element.
  const container = document.createElement('div')
  container.className = TABSET_CONTAINER_CLASS
  wrapElements(allSetNodes, container)

  // Apply template to tabset container element.
  container.innerHTML = Handlebars.templates.tabset({tabs: tabSet})

  // Return tabset container element.
  return container
}

/**
 * Determines whether or not a DOM node is treated as a tabset closer marker.
 * @param {Node} node A DOM node.
 * @returns {Boolean}
 */
function isTabSetCloser (node) {
  return node.nodeName === '#comment' && node.nodeValue.trim() === TABSET_CLOSE_COMMENT
}

/**
 * Pushes panel data object to tabset.
 * The setIndex is used to provide each tabset’s elements unique IDs.
 *
 * @param {Object} panel The panel object to push.
 * @param {Array} set The parent tabset.
 * @param {Integer} setIndex The parent tabset index.
 */
function pushPanel (panel, set, setIndex) {
  // If panel data is incomplete, do not push. (Usually the case when the first
  // tab panel heading is encountered.)
  if (panel.label === '' && !panel.content.length) return false

  const label = panel.label
  const content = panel.content
  set.push({label, content, setIndex})
}

/**
 * Wraps elements with the wrapper element.
 *
 * @param {Array} elements The elements to wrap.
 * @param {Element} wrapper The wrapping element.
 */
function wrapElements (elements, wrapper) {
  if (!elements || !elements.length) return false

  elements[0].parentNode.insertBefore(wrapper, elements[0])
  elements.forEach((el) => wrapper.appendChild(el))
}

/**
 * Adds behaviour to a processed tabset.
 *
 * @param {Element} set A processed tabset container element.
 */
function activateTabset (set) {
  // Register tab buttons and panels, and create initial state object.
  const state = {
    tabs: set.querySelectorAll(':scope [role="tab"]'),
    panels: set.querySelectorAll(':scope [role="tabpanel"]'),
    activeIndex: 0
  }

  state.tabs.forEach((tab, index) => {
    // Pointing/touch device.
    tab.addEventListener('click', (_event) => {
      setActiveTab(index, state)
    })

    // Keyboard (arrow keys should wrap from first to last and vice-versa).
    tab.addEventListener('keydown', (event) => {
      const lastIndex = state.tabs.length - 1

      if (event.code === 'ArrowLeft') {
        event.preventDefault()
        if (state.activeIndex === 0) {
          setActiveTab(lastIndex, state)
        } else {
          setActiveTab(state.activeIndex - 1, state)
        }
      } else if (event.code === 'ArrowRight') {
        event.preventDefault()
        if (state.activeIndex === lastIndex) {
          setActiveTab(0, state)
        } else {
          setActiveTab(state.activeIndex + 1, state)
        }
      } else if (event.code === 'Home') {
        event.preventDefault()
        setActiveTab(0, state)
      } else if (event.code === 'End') {
        event.preventDefault()
        setActiveTab(lastIndex, state)
      }
    })
  })
}

/**
 * Updates active tab button and panel. Covers ARIA attributes and
 * (de)activates tab navigation.
 *
 * @param {Integer} index
 * @param {Object} state
 */
function setActiveTab (index, state) {
  // Set previously active tab button as inactive.
  state.tabs[state.activeIndex].setAttribute('aria-selected', 'false')
  state.tabs[state.activeIndex].tabIndex = -1

  // Set newly active tab button as active.
  state.tabs[index].setAttribute('aria-selected', 'true')
  state.tabs[index].tabIndex = 0
  state.tabs[index].focus()

  // Set previously active tab panel as inactive.
  state.panels[state.activeIndex].setAttribute('hidden', '')
  state.panels[state.activeIndex].tabIndex = -1

  // Set newly active tab panel as active.
  state.panels[index].removeAttribute('hidden')
  state.panels[index].tabIndex = 0

  // Update state's active index.
  state.activeIndex = index
}
