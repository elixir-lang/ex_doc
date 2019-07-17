// sidebar versions dropdown
/* globals versionNodes */

// Dependencies
// ------------
import $ from 'jquery'
import find from 'lodash.find'
import versionsTemplate from './templates/versions-dropdown.handlebars'

// Local Variables
// ---------------

const sidebarProjectVersion = $('.sidebar-projectVersion')
const currentVersion = sidebarProjectVersion.text().trim()

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
    var width = $('.sidebar-projectVersion').width() + 10

    let versionsDropdown = versionsTemplate({nodes: versionNodes.map(addIsCurrentVersion)})
    sidebarProjectVersion.text('')
    sidebarProjectVersion.append(versionsDropdown)

    $('.sidebar-projectVersionsDropdown').width(width).change(function () {
      window.location.href = $(this).val()
    })
  }
}
