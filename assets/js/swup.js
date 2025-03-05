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
  const match = page.html.match(/<meta\s+http-equiv\s*=\s*["']refresh["']\s+content\s*=\s*["']\d+\s*;\s*url\s*=\s*([^"']+)["']/i)

  if (match && match[1]) {
    visit.abort()
    window.location.href = match[1]
  }
}

window.addEventListener('DOMContentLoaded', emitExdocLoaded)

if (!isEmbedded && window.location.protocol !== 'file:') {
  new Swup({
    animationSelector: false,
    containers: ['#main'],
    ignoreVisit: (url) => {
      const path = url.split('#')[0]
      return path === window.location.pathname ||
            path === window.location.pathname + '.html'
    },
    linkSelector: 'a[href]:not([href^="/"]):not([href^="http"])',
    hooks: {
      'page:load': maybeMetaRedirect,
      'page:view': emitExdocLoaded
    },
    plugins: [new SwupA11yPlugin(), new SwupProgressPlugin({delay: 500})]
  })
}
