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
// Routing non-HTML files (downloads such as .mmd) through SWUP breaks the page
// swap (#2182); the `.html#` branch keeps in-doc anchored links (function
// references like file.html#list_dir/1, sidebar entries) on the SWUP path.
export const LINK_SELECTOR = 'a[href]:not([href^="/"]):not([href^="http"]):is([href$=".html"], [href*=".html#"])'

// SWUP only swaps `#main`, so it must stay within a single ExDoc build: the
// sidebar (navigation + version) is built from per-build <head> scripts SWUP
// does not re-run. ExDoc writes every page of a build as a flat file in one
// folder, so an in-build link is a bare filename and a slash in its path means
// another folder/build (the Erlang/OTP docs flatten one build per application).
// We test the path -- the part before the fragment -- not the whole href, since
// anchors carry slashes too (the arity in file.html#list_dir/1).
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
