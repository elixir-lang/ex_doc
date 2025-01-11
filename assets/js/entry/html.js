// Load preview & hint-page first because they could remove DOM.
// This prevents later modules doing unnecessary work.
import '../preview'
import '../tooltips/hint-page'
// The remaining modules are loaded in order of visible impact.
import '../theme'
import '../sidebar/sidebar-drawer'
import '../sidebar/sidebar-version-select'
import '../tabsets'
import '../content'
import '../code'
import '../search-bar'
import '../tooltips/tooltips'
import '../copy-button'
import '../search-page'
import '../settings'
import '../keyboard-shortcuts'
import '../quick-switch'
import '../swup'
