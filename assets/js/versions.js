// sidebar versions dropdown

// Dependencies
// ------------
import $ from 'jquery'


function version_option (current_version) {
  return function (version_object) {
    return '<option ' + option_attributes(version_object, version_object.version == current_version) + '>' +
      version_object.version +
      '</option>'
  }
}

function option_attributes (version_object, is_current_version) {
  return `value="${version_object.url}"${(is_current_version ? ' selected disabled' : '')}`
}

// Public Methods
// --------------
export function initialize() {
  if (typeof versionNodes !== 'undefined') {
    let sidebar_projectVersion = $('.sidebar-projectVersion')
    let current_version = sidebar_projectVersion.text().trim()

    if (!versionNodes.find(function (element) { return element.version == current_version })) {
      versionNodes.unshift({ version: current_version, url: '#' })
    }

    let versions_dropdown =
      '<form autocomplete="off">' +
      '<select class="sidebar-projectVersionsDropdown">' +
      (versionNodes.map(version_option(current_version)).join('')) +
      '</select>' +
      '</form>'
    sidebar_projectVersion.text('')
    sidebar_projectVersion.append(versions_dropdown)

    $('.sidebar-projectVersionsDropdown').change(function () {
      window.location.href = $(this).val();
    })
  }
}
