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
 * Prepares data in HTML for the tabset template, wraps the tabset
 * elements to enable replacement with result of template,
 * and removes the open/close comment nodes.
 *
 * @param {Element} element A tabset opener comment node.
 * @param {Integer} tabSetIndex
 * @param {Array} array
 * @returns {Array} Structured tabset data.
 */
function processTabset (element, tabSetIndex, _array) {
  const setNodes = []
  const tabSet = []
  const tabPanel = {
    label: '',
    content: []
  }

  while ((element = element.nextSibling)) {
    if (isTabSetCloser(element)) {
      pushPanel(tabPanel, tabSet, tabSetIndex)
      break
    }

    setNodes.push(element)

    if (element.nodeName === TABPANEL_HEADING_NODENAME) {
      pushPanel(tabPanel, tabSet, tabSetIndex)
      tabPanel.label = element.innerText
      tabPanel.content = []
    } else {
      tabPanel.content.push(element.outerHTML)
    }
  }

  const wrapper = document.createElement('div')
  wrapper.className = TABSET_CONTAINER_CLASS
  wrapElements(setNodes, wrapper)

  wrapper.innerHTML = Handlebars.templates.tabset({tabs: tabSet})

  return wrapper
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
 *
 * @param {Object} panel The panel object to push.
 * @param {Array} set The parent tabset.
 * @param {Integer} setIndex The parent tabset index. (Enables each tabset’s elements to have unique IDs.
 */
function pushPanel (panel, set, setIndex) {
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
  const state = {
    tabs: set.querySelectorAll(':scope [role="tab"]'),
    panels: set.querySelectorAll(':scope [role="tabpanel"]'),
    activeIndex: 0
  }

  state.tabs.forEach((tab, index) => {
    // Pointing device
    tab.addEventListener('click', (_event) => {
      setActiveTab(index, state)
    })

    // Keyboard (arrow keys should wrap from first to last and vice-versa)
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
 * Sets the active tab by updating tab and panel elements, before updating the
 * state object’s activeIndex property.
 *
 * @param {Integer} index
 * @param {Object} state
 */
function setActiveTab (index, state) {
  state.tabs[state.activeIndex].setAttribute('aria-selected', 'false')
  state.tabs[state.activeIndex].tabIndex = -1
  state.tabs[index].setAttribute('aria-selected', 'true')
  state.tabs[index].tabIndex = 0
  state.tabs[index].focus()

  state.panels[state.activeIndex].setAttribute('hidden', '')
  state.panels[state.activeIndex].tabIndex = -1
  state.panels[index].removeAttribute('hidden')
  state.panels[index].tabIndex = 0

  state.activeIndex = index
}
