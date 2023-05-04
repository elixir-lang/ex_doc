import { qsAll } from './helpers'

const TABSET_OPEN_SELECTOR = 'hr.tabs-open'
const TABSET_CLOSE_SELECTOR = 'hr.tabs-close'
const TABPANEL_HEADING_SELECTOR = 'h3'
const TABSET_CONTAINER_CLASS = 'tabset'

export function initialize () {
  const tabsetOpeners = Array.from(qsAll(TABSET_OPEN_SELECTOR))
  const tabsetContainers = tabsetOpeners.map(processTabset)
  tabsetContainers.forEach((set) => activateTabset(set))
  removeRules()
}

/**
 * Prepares data in HTML for the tabset template, wraps the tabset
 * elements to enable replacement with result of template,
 * and removes the open/close marker elements.
 *
 * @param {Element} el A tabset open marker element.
 * @returns {Array} Structured tabset data.
 */
function processTabset (el, index, _array) {
  const setElements = []
  const set = []
  const panel = {
    label: '',
    content: []
  }

  while ((el = el.nextElementSibling)) {
    if (el.matches(TABSET_CLOSE_SELECTOR)) {
      pushPanel(panel, set, index)
      break
    }

    setElements.push(el)

    if (el.matches(TABPANEL_HEADING_SELECTOR)) {
      pushPanel(panel, set, index)
      panel.label = el.innerText
      panel.content = []
    } else {
      panel.content.push(el.outerHTML)
    }
  }

  // Wrap tabset elements in new tabset div
  const wrapper = document.createElement('div')
  wrapper.className = TABSET_CONTAINER_CLASS
  wrapElements(setElements, wrapper)

  wrapper.innerHTML = Handlebars.templates.tabset({tabs: set})

  return wrapper
}

/**
 * Removes opening and closing horizontal rule elements
 */
function removeRules () {
  const tabsetRules = qsAll(`${TABSET_OPEN_SELECTOR}, ${TABSET_CLOSE_SELECTOR}`)
  tabsetRules.forEach(rule => rule.remove())
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
  if (elements && elements.length) {
    elements[0].parentNode.insertBefore(wrapper, elements[0])
    elements.forEach((el) => wrapper.appendChild(el))
  }
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
