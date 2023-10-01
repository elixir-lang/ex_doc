import '../handlebars/helpers'

import { onDocumentReady } from '../helpers'
import { initialize as initContent } from '../content'
import { initialize as initSidebarDrawer } from '../sidebar/sidebar-drawer'
import { initialize as initSidebarContent } from '../sidebar/sidebar-list'
import { initialize as initSidebarSearch } from '../sidebar/sidebar-search'
import { initialize as initVersions } from '../sidebar/sidebar-version-select'
import { initialize as initSearchPage } from '../search-page'
import { initialize as initTheme } from '../theme'
import { initialize as initMakeup } from '../makeup'
import { initialize as initModal } from '../modal'
import { initialize as initKeyboardShortcuts } from '../keyboard-shortcuts'
import { initialize as initQuickSwitch } from '../quick-switch'
import { initialize as initToast } from '../toast'
import { initialize as initTooltips } from '../tooltips/tooltips'
import { initialize as initHintsPage } from '../tooltips/hint-page'
import { initialize as initCopyButton } from '../copy-button'
import { initialize as initSettings } from '../settings'
import { initialize as initOs } from '../os'
import { initialize as initTabsets } from '../tabsets'

onDocumentReady(() => {
  initTheme()
  initSidebarDrawer()
  initSidebarContent()
  initSidebarSearch()
  initVersions()
  initContent()
  initMakeup()
  initModal()
  initKeyboardShortcuts()
  initQuickSwitch()
  initToast()
  initTooltips()
  initHintsPage()
  initSearchPage()
  initCopyButton()
  initSettings()
  initOs()
  initTabsets()
})
