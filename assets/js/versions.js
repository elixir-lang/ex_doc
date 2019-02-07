// sidebar versions dropdown

// Dependencies
// ------------
import $ from 'jquery'

function versionOption (currentVersion) {
  return function (versionObject) {
    return '<option ' + optionAttributes(versionObject, versionObject.version === currentVersion) + '>' +
      versionObject.version +
      '</option>'
  }
}

function optionAttributes (versionObject, isCurrentVersion) {
  return `value="${versionObject.url}"${(isCurrentVersion ? ' selected disabled' : '')}`
}

// Public Methods
// --------------

/* eslint-disable no-undef */
export function initialize () {
  if (typeof versionNodes !== 'undefined') {
    let sidebarProjectVersion = $('.sidebar-projectVersion')
    let currentVersion = sidebarProjectVersion.text().trim()

    if (!versionNodes.find(function (element) { return element.version === currentVersion })) {
      versionNodes.unshift({ version: currentVersion, url: '#' })
    }

    let versionsDropdown =
      '<form autocomplete="off">' +
      '<select class="sidebar-projectVersionsDropdown">' +
      (versionNodes.map(versionOption(currentVersion)).join('')) +
      '</select>' +
      '</form>'
    sidebarProjectVersion.text('')
    sidebarProjectVersion.append(versionsDropdown)

    $('.sidebar-projectVersionsDropdown').change(function () {
      window.location.href = $(this).val()
    })
  }
}
/* eslint-enable no-undef */
