import { qsAll } from './helpers'
import { settingsStore } from './settings-store'

/**
 * Updates "Run in Livebook" badges to link to a notebook
 * corresponding to the current documentation page.
 */

window.addEventListener('swup:page:view', initialize)
initialize()

function initialize () {
  const notebookPath = window.location.pathname.replace(/(\.html)?$/, '.livemd')
  const notebookUrl = encodeURIComponent(new URL(notebookPath, window.location.href).toString())

  settingsStore.getAndSubscribe(({livebookUrl}) => {
    const targetUrl = livebookUrl
      ? `${livebookUrl}/import?url=${notebookUrl}`
      : `https://livebook.dev/run?url=${notebookUrl}`

    for (const anchor of qsAll('.livebook-badge')) {
      anchor.href = targetUrl
    }
  })
}
