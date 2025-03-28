import { el, getCurrentPageSidebarType, qs, qsAll } from '../helpers'
import { getSidebarNodes } from '../globals'

// Sidebar list is only rendered when needed.
// Mobile users may never see the sidebar.
let init = false
export function initialize () {
  if (init) return
  init = true

  const sidebarList = document.getElementById('sidebar-list-nav')

  if (!sidebarList) return

  const defaultTab = getCurrentPageSidebarType()
  const tabs = {
    extras: sidebarList.dataset.extras || 'Pages',
    modules: 'Modules',
    tasks: '<span translate="no">Mix</span> Tasks'
  }

  Object.entries(tabs).forEach(([type, titleHtml]) => {
    const nodes = getSidebarNodes()[type]

    if (!nodes?.length) return

    const tabId = `${type}-list-tab-button`
    const tabpanelId = `${type}-tab-panel`
    const selected = type === defaultTab

    const tab = el('button', {
      id: tabId,
      role: 'tab',
      tabindex: selected ? 0 : -1,
      'aria-selected': selected || undefined,
      'aria-controls': tabpanelId
    })
    tab.innerHTML = titleHtml
    tab.addEventListener('keydown', handleTabKeydown)
    tab.addEventListener('click', handleTabClick)
    sidebarList.appendChild(el('li', {}, [tab]))

    const nodeList = el('ul', {class: 'full-list'})
    nodeList.addEventListener('click', handleNodeListClick)

    const tabpanel = el('div', {
      id: tabpanelId,
      class: 'sidebar-tabpanel',
      role: 'tabpanel',
      'aria-labelledby': tabId,
      hidden: selected ? undefined : ''
    }, [nodeList])
    document.getElementById('sidebar').appendChild(tabpanel)

    let group = ''
    let nestedContext
    let lastModule
    nodeList.replaceChildren(...nodes.flatMap(node => {
      const items = []
      const hasHeaders = Array.isArray(node.headers)
      const translate = hasHeaders ? undefined : 'no'

      // Group header.
      if (node.group !== group) {
        items.push(el('li', {class: 'group', translate}, [node.group]))
        group = node.group
        nestedContext = undefined
      }

      // Nesting context.
      if (node.nested_context && node.nested_context !== nestedContext) {
        nestedContext = node.nested_context
        if (lastModule !== nestedContext) {
          items.push(el('li', {class: 'nesting-context', translate: 'no', 'aria-hidden': true}, [nestedContext]))
        }
      } else {
        lastModule = node.title
      }

      items.push(el('li', {}, [
        el('a', {href: `${node.id}.html`, translate}, [node.nested_title || node.title]),
        ...childList(`node-${node.id}-headers`,
          hasHeaders
            ? renderHeaders(node)
            : renderSectionsAndGroups(node)
        )
      ]))

      return items
    }))
  })

  // Update new sidebar list with current hash.
  markCurrentHashInSidebar()

  // Triggers layout, defer.
  requestAnimationFrame(scrollNodeListToCurrentCategory)

  // Keep updated with future changes.
  window.addEventListener('hashchange', markCurrentHashInSidebar)
  window.addEventListener('exdoc:loaded', markCurrentHashInSidebar)
}

/**
 * @param {string} id
 * @param {HTMLElement[]} childItems
 */
function childList (id, childItems) {
  if (!childItems.length) return []

  return [
    el('button', {'aria-label': 'expand', 'aria-expanded': false, 'aria-controls': id}),
    el('ul', {id}, childItems)
  ]
}

function renderHeaders (node) {
  return node.headers
    .map(({id, anchor}) =>
      el('li', {}, [
        el('a', {href: `${node.id}.html#${anchor}`}, [id])
      ])
    )
}

function renderSectionsAndGroups (node) {
  const items = []

  if (node.sections?.length) {
    items.push(el('li', {}, [
      el('a', {href: `${node.id}.html#content`}, ['Sections']),
      ...childList(`${node.id}-sections-list`,
        node.sections
          .map(({id, anchor}) =>
            el('li', {}, [
              el('a', {href: `${node.id}.html#${anchor}`}, [id])
            ])
          )
      )
    ]))
  }

  if (node.nodeGroups) {
    items.push(el('li', {}, [
      el('a', {href: `${node.id}.html#summary`}, ['Summary'])
    ]))

    items.push(...node.nodeGroups.map(({key, name, nodes}) =>
      el('li', {}, [
        el('a', {href: `${node.id}.html#${key}`}, [name]),
        ...childList(`node-${node.id}-group-${key}-list`,
          nodes
            .map(({anchor, title, id}) =>
              el('li', {}, [
                el('a', {href: `${node.id}.html#${anchor}`, title, translate: 'no'}, [id])
              ])
            )
        )
      ])
    ))
  }

  return items
}

/** @param {HTMLButtonElement} */
function activateTab (next) {
  const prev = document.getElementById('sidebar-list-nav').querySelector('[aria-selected]')

  if (prev === next) return

  if (prev) {
    prev.removeAttribute('aria-selected')
    prev.setAttribute('tabindex', '-1')
    document.getElementById(prev.getAttribute('aria-controls')).setAttribute('hidden', 'hidden')
  }

  next.setAttribute('aria-selected', 'true')
  next.setAttribute('tabindex', '0')
  document.getElementById(next.getAttribute('aria-controls')).removeAttribute('hidden')
}

function scrollNodeListToCurrentCategory () {
  qs('#sidebar [role=tabpanel]:not([hidden]) a[aria-selected]')?.scrollIntoView()
}

function markCurrentHashInSidebar () {
  const sidebar = document.getElementById('sidebar')
  const {pathname, hash} = window.location

  // All sidebar links are relative and end in .html.
  const page = pathname.split('/').pop().replace(/\.html$/, '') + '.html'

  // Try find exact link with hash, fall back to page.
  const current = sidebar.querySelector(`li a[href="${page + hash}"]`) || sidebar.querySelector(`li a[href="${page}"]`)

  if (!current) return

  // Unset previous.
  sidebar.querySelectorAll('.full-list a[aria-selected]').forEach(element => {
    element.removeAttribute('aria-selected')
  })

  // Close open menus.
  sidebar.querySelectorAll('.full-list button[aria-expanded=true]').forEach(element => {
    element.setAttribute('aria-expanded', false)
  })

  // Walk up parents, updating link, button and tab attributes.
  let element = current.parentElement
  while (element) {
    if (element.tagName === 'LI') {
      const link = element.firstChild
      link.setAttribute('aria-selected', link.getAttribute('href') === page ? 'page' : 'true')
      const button = link.nextSibling
      if (button?.tagName === 'BUTTON') {
        button.setAttribute('aria-expanded', true)
      }
    } else if (element.role === 'tabpanel') {
      if (element.hasAttribute('hidden')) {
        activateTab(document.getElementById(element.getAttribute('aria-labelledby')))
      }
      break
    }
    element = element.parentElement
  }
}

/**
 * Provide left/right arrow navigation for tablist, as required by ARIA authoring practices guide.
 *
 * @param {KeyboardEvent}
 **/
function handleTabKeydown (event) {
  if (!['ArrowRight', 'ArrowLeft'].includes(event.key)) { return }

  const tabs = Array.from(qsAll('#sidebar-list-nav [role="tab"]'))
  const currentIndex = tabs.indexOf(event.currentTarget)
  const nextIndex = currentIndex + (event.key === 'ArrowRight' ? 1 : -1)
  const nextTab = tabs.at(nextIndex % tabs.length)

  activateTab(nextTab)
  nextTab.focus()
}

/** @param {MouseEvent} */
function handleTabClick (event) {
  activateTab(event.currentTarget)
  scrollNodeListToCurrentCategory()
}

/** @param {MouseEvent} */
function handleNodeListClick (event) {
  const target = event.target

  if (target.tagName === 'BUTTON') {
    target.setAttribute('aria-expanded', target.getAttribute('aria-expanded') === 'false')
  }
}
