import { qs, checkUrlExists } from '../helpers'
import { getVersionNodes, isEmbedded } from '../globals'
import versionsDropdownTemplate from '../handlebars/templates/versions-dropdown.handlebars'

const VERSIONS_CONTAINER_SELECTOR = '.sidebar-projectVersion'
const VERSIONS_DROPDOWN_SELECTOR = '.sidebar-projectVersionsDropdown'

/**
 * Initializes selectable version list if `versionNodes` have been configured.
 */

if (!isEmbedded) {
  const versionNodes = getVersionNodes()
  const versionsContainer = qs(VERSIONS_CONTAINER_SELECTOR)

  if (versionNodes.length > 0 || !versionsContainer) {
    // Initially the container contains only text with the current version
    const currentVersion = versionsContainer.textContent.trim()
    // Add the current version node to the list if not there.
    const withCurrentVersion = versionNodes.some((node) => node.version === currentVersion)
      ? versionNodes
      : [{ version: currentVersion, url: '#' }, ...versionNodes]
    // Add additional attributes to version nodes for rendering.
    const nodes = withCurrentVersion.map(node => ({
      ...node,
      isCurrentVersion: node.version === currentVersion
    }))

    versionsContainer.innerHTML = versionsDropdownTemplate({ nodes })

    qs(VERSIONS_DROPDOWN_SELECTOR).addEventListener('change', handleVersionSelected)
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
