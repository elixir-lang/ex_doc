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

/**
 * Checks if a navigation should be ignored by swup.
 * Returns true for same-page links and cross-directory links.
 * Same-directory links get smooth swup transitions.
 */
export function shouldIgnoreVisit (url, currentPathname) {
  const path = url.split('#')[0]
  // Ignore same-page links (with or without .html mismatch).
  if (path === currentPathname ||
      path === currentPathname + '.html' ||
      path + '.html' === currentPathname) { return true }
  // Only use swup for links within the same directory (same app).
  // Cross-directory links need a full reload to update the sidebar.
  const currentDir = currentPathname.substring(0, currentPathname.lastIndexOf('/') + 1)
  const targetDir = path.substring(0, path.lastIndexOf('/') + 1)
  return targetDir !== currentDir
}

window.addEventListener('DOMContentLoaded', emitExdocLoaded)

if (!isEmbedded && window.location.protocol !== 'file:') {
  new Swup({
    animationSelector: false,
    containers: ['#main'],
    ignoreVisit: (url) => shouldIgnoreVisit(url, window.location.pathname),
    linkSelector: 'a[href]:not([href^="http"]):not([href^="#"])',
    hooks: {
      'page:load': maybeMetaRedirect,
      'page:view': emitExdocLoaded
    },
    plugins: [new SwupA11yPlugin(), new SwupProgressPlugin({delay: 500})]
  })
}
