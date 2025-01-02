import { onDocumentReady } from '../helpers'
import { initialize as initTabsets } from '../tabsets'
import { initialize as initContent } from '../content'
import { initialize as initSidebarDrawer, update as updateSidebarDrawer } from '../sidebar/sidebar-drawer'
import { initialize as initSidebarContent, update as updateSidebarContent } from '../sidebar/sidebar-list'
import { initialize as initSearch } from '../search-bar'
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
import { initialize as initStyling } from '../styling'
import { initialize as initPreview} from '../preview'

import Swup from 'swup'
import SwupA11yPlugin from '@swup/a11y-plugin'
import SwupProgressPlugin from '@swup/progress-plugin'

onDocumentReady(() => {
  const params = new URLSearchParams(window.location.search)
  const isEmbedded = window.self !== window.parent
  const isPreview = params.has('preview')
  const isHint = params.has('hint')

  initTheme()
  initStyling()

  initTabsets()
  initContent(isPreview)
  initMakeup()
  initTooltips()
  initCopyButton()

  if (isPreview && isEmbedded) {
    initPreview()
  } if (isHint && isEmbedded) {
    initHintsPage()
  } else {
    if (window.location.protocol !== 'file:') {
      new Swup({
        animationSelector: false,
        containers: ['#main'],
        ignoreVisit: (url) => {
          const path = url.split('#')[0]
          return path === window.location.pathname ||
            path === window.location.pathname + '.html'
        },
        hooks: {
          'page:view': () => {
            initTabsets()
            initContent(false)
            initMakeup()
            initTooltips()
            initCopyButton()

            updateSidebarDrawer()
            updateSidebarContent()
            initSearch()
            initSearchPage()
            initSettings()
          }
        },
        linkSelector: 'a[href]:not([href^="/"]):not([href^="http"])',
        plugins: [new SwupA11yPlugin(), new SwupProgressPlugin({delay: 500})]
      })
    }

    initVersions()
    initModal()
    initKeyboardShortcuts()
    initQuickSwitch()
    initToast()

    initSidebarDrawer()
    initSidebarContent()
    initSearch()
    initSearchPage()
    initSettings()
  }
})
