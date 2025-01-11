/* globals Prism */
import { el, qsAll } from './helpers'
import { isDark } from './theme'

const prismBaseUrl = 'https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0'
// Prevent auto highlighting.
window.Prism = {manual: true}

/**
 * Sets up syntax highlighting in code blocks.
 */

window.addEventListener('swup:page:view', initialize)
initialize()

export function initialize () {
  qsAll('pre > code').forEach(element => {
    loadPrism().then(() => {
      Prism.highlightElement(element)
    })
  })
}

/** @type {Promise<void>} */
let loadPromise

function loadPrism () {
  if (!loadPromise) {
    const theme = isDark() ? '-okaidia' : ''
    load(`themes/prism${theme}.min.css`)
    loadPromise = load('prism.min.js', [
      'components/prism-elixir.min.js',
      'components/prism-erlang.min.js',
      'plugins/autoloader/prism-autoloader.min.js'
    ])
  }
  return loadPromise
}

function load (path, dependents) {
  const loaded = new Promise((resolve) => {
    const element = path.endsWith('.css')
      ? el('link', {
        crossorigin: 'anonymous',
        referrerpolicy: 'no-referrer',
        rel: 'stylesheet',
        href: `${prismBaseUrl}/${path}`
      })
      : el('script', {
        defer: '',
        crossorigin: 'anonymous',
        referrerpolicy: 'no-referrer',
        src: `${prismBaseUrl}/${path}`
      })
    element.addEventListener('load', resolve)
    document.head.append(element)
  })

  if (Array.isArray(dependents)) {
    return loaded.then(() => Promise.all(dependents.map(load)))
  }

  return loaded
}
