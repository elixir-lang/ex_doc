// sidebar versions dropdown
/* globals versionNodes */

// Dependencies
// ------------
import find from 'lodash.find'
import versionsTemplate from './templates/versions-dropdown.handlebars'
import {qs, checkUrlExists} from './helpers'

// Local Variables
// ---------------

const sidebarProjectVersion = qs('.sidebar-projectVersion')
const currentVersion = sidebarProjectVersion.textContent.trim()

// Local Methods
// -------------

function addCurrentVersionIfNotPresent () {
  if (!find(versionNodes, function (element) { return element.version === currentVersion })) {
    versionNodes.unshift({ version: currentVersion, url: '#' })
  }
}

function addIsCurrentVersion (element) {
  element.isCurrentVersion = element.version === currentVersion
  return element
}

// Public Methods
// --------------

export function initialize () {
  if (typeof versionNodes !== 'undefined') {
    addCurrentVersionIfNotPresent()
    const width = qs('.sidebar-projectVersion').offsetWidth + 10

    const versionsDropdown = versionsTemplate({nodes: versionNodes.map(addIsCurrentVersion)})
    sidebarProjectVersion.innerHTML = versionsDropdown

    const dropdown = qs('.sidebar-projectVersionsDropdown')
    dropdown.style.width = `${width}px`
    dropdown.addEventListener('change', function (event) {
      const url = event.target.value
      const otherVersionWithPath = url + '/' + window.location.href.split('/').pop()
      checkUrlExists(otherVersionWithPath)
        .then(exists => {
          if (exists) {
            window.location.href = otherVersionWithPath
          } else {
            window.location.href = url
          }
        })
    })
  }
}
