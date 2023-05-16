import { qs, checkUrlExists } from '../helpers'
import { getVersionNodes } from '../globals'

const VERSIONS_CONTAINER_SELECTOR = '.sidebar-projectVersion'
const VERSIONS_DROPDOWN_SELECTOR = '.sidebar-projectVersionsDropdown'
const VERSIONS_CURRENT_BUTTON = ".sidebar-projectLatestVersionButton"

/**
 * Initializes selectable version list if `versionNodes` have been configured.
 */
export function initialize () {
  const versionNodes = getVersionNodes()

  if (versionNodes.length > 0) {
    const versionsContainer = qs(VERSIONS_CONTAINER_SELECTOR)
    // Initially the container contains only text with the current version
    const currentVersion = versionsContainer.textContent.trim()
    const nodes = decorateVersionNodes(versionNodes, currentVersion)

    renderVersionsDropdown({ nodes })
  }
}

function renderVersionsDropdown ({ nodes }) {
  const versionsContainer = qs(VERSIONS_CONTAINER_SELECTOR)
  const versionsDropdownHtml = Handlebars.templates['versions-dropdown']({ nodes })
  versionsContainer.innerHTML = versionsDropdownHtml

  qs(VERSIONS_DROPDOWN_SELECTOR).addEventListener('change', handleVersionSelected)
  if (qs(VERSIONS_CURRENT_BUTTON)) {
    qs(VERSIONS_CURRENT_BUTTON).addEventListener('click', handleVersionSelected)
  }
}

/**
 * Adds additional attributes to version nodes for rendering.
 */
function decorateVersionNodes (nodes, currentVersion) {
  const withCurrentVersion = ensureCurrentVersionNode(nodes, currentVersion)
  const withSingleLatest = ensureSingleLatestNode(withCurrentVersion)

  return withSingleLatest.map(node => ({
    ...node,
    isCurrentVersion: node.version === currentVersion
  }))
}

/**
 * Ensures that the node list only has one entry with latest: true
 */
function ensureSingleLatestNode (nodes) {
  var seenLatest = false

  return nodes.map((node) => {
    if (seenLatest){
      return {...node, latest: false}
    } else {
      seenLatest = node.latest ? true : false
      return {...node, latest: seenLatest}
    }
  })
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
