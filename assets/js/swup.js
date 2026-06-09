import Swup from 'swup'
import SwupA11yPlugin from '@swup/a11y-plugin'
import SwupProgressPlugin from '@swup/progress-plugin'
import { isEmbedded } from './globals'

// Emit exdoc:loaded each time content loads:
// - on initial page load (DOMContentLoaded)
// - on subsequent SWUP page loads (page:view)
const emitExdocLoaded = () => {
  window.dispatchEvent(new Event('exdoc:loaded'))
}

const maybeMetaRedirect = (visit, {page}) => {
  const hasMetaRefresh = /<meta\s+http-equiv\s*=\s*["']refresh["']\s+content\s*=\s*["'][^"']*["']\s*\/?>/i.test(page.html)

  if (hasMetaRefresh) {
    visit.abort()
    window.location.reload()
  }
}

window.addEventListener('DOMContentLoaded', emitExdocLoaded)

// Match links to local .html documentation pages, with or without a fragment.
// Note we need to explicitly check for .html so we don't use swup on other
// formats, such as .md, and so forth.
//
// We want to only apply SWUP on links without "/" (or rather "/" can only
// appear in a fragment), but it is not possible to write such selector,
// so we check specifically for root relative links (starting with "/" or
// starting with http) in the LINK_SELECTOR and then reject any link with
// "/" before the fragment in "isWithinBuild".
export const LINK_SELECTOR = 'a[href]:not([href^="/"]):not([href^="http"]):is([href$=".html"], [href*=".html#"])'
export const isWithinBuild = (href) => !href.split('#')[0].includes('/')

if (!isEmbedded && window.location.protocol !== 'file:') {
  new Swup({
    animationSelector: false,
    containers: ['#main'],
    ignoreVisit: (url, {el} = {}) => {
      const path = url.split('#')[0]
      return path === window.location.pathname ||
            path === window.location.pathname + '.html' ||
            !isWithinBuild(el?.getAttribute('href') ?? '')
    },
    linkSelector: LINK_SELECTOR,
    hooks: {
      'page:load': maybeMetaRedirect,
      'page:view': emitExdocLoaded
    },
    plugins: [new SwupA11yPlugin(), new SwupProgressPlugin({delay: 500})]
  })
}
