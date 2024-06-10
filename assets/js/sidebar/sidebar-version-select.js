import { qs, checkUrlExists } from '../helpers'
import { getVersionNodes } from '../globals'

const VERSIONS_CONTAINER_SELECTOR = '.sidebar-projectVersion'
const VERSIONS_DROPDOWN_SELECTOR = '.sidebar-projectVersionsDropdown'

/**
 * Initializes selectable version list if `versionNodes` have been configured.
 */
export function initialize () {
  const versionNodes = getVersionNodes()

  if (versionNodes.length > 0) {
    const versionsContainer = qs(VERSIONS_CONTAINER_SELECTOR)
    // Initially the container contains only text with the current version
    const currentVersion = versionsContainer.textContent.trim()
    const latestVersionNode = versionNodes.find(node => node.latest)
    const jumpToLatestUrl = latestVersionNode?.version !== currentVersion ? latestVersionNode?.url : null

    const nodes = decorateVersionNodes(versionNodes, currentVersion)

    renderVersionPicker(nodes, jumpToLatestUrl)
  }
}

function renderVersionPicker (nodes, jumpToLatestUrl) {
  const versionsContainer = qs(VERSIONS_CONTAINER_SELECTOR)
  const versionPickerHtml = Handlebars.templates['versions-dropdown']({ nodes, jumpToLatestUrl })
  versionsContainer.innerHTML = versionPickerHtml

  qs(VERSIONS_DROPDOWN_SELECTOR).addEventListener('change', handleVersionSelected)
}

/**
 * Adds additional attributes to version nodes for rendering.
 */
function decorateVersionNodes (nodes, currentVersion) {
  const withCurrentVersion = ensureCurrentVersionNode(nodes, currentVersion)

  return withCurrentVersion.map(node => ({
    ...node,
    isCurrentVersion: node.version === currentVersion
  }))
}

/**
 * Adds the current version node to the list unless it's already there.
 */
function ensureCurrentVersionNode (nodes, currentVersion) {
  const currentVersionPresent = nodes.some(
    (node) => node.version === currentVersion
  )

  if (currentVersionPresent) {
    return nodes
  } else {
    const currentVersionNode = { version: currentVersion, url: '#' }
    return [currentVersionNode, ...nodes]
  }
}

function handleVersionSelected (event) {
  const url = event.target.value
  const pathSuffix = window.location.pathname.split('/').pop() + window.location.hash
  const otherVersionWithPath = `${url}/${pathSuffix}`

  checkUrlExists(otherVersionWithPath)
    .then(exists => {
      if (exists) {
        window.location.href = otherVersionWithPath
      } else {
        window.location.href = url
      }
    })
}

/**
 * Opens the version select if available.
 * Only focuses the version select if
 *   - the browser's HTMLSelectElement lacks the showPicker method
 *   - there has been transient user interaction
 */
export function openVersionSelect () {
  const select = qs(VERSIONS_DROPDOWN_SELECTOR)

  if (select) {
    select.focus()

    // Prevent subsequent 'v' press from submitting form
    select.addEventListener('keydown', event => {
      if (event.key === 'Escape' || event.key === 'v') {
        event.preventDefault()
        select.blur()
      }
    })

    if (navigator.userActivation.isActive && 'showPicker' in HTMLSelectElement.prototype) {
      select.showPicker()
    }
  }
}
