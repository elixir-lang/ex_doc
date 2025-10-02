import { isEmbedded, isPreview } from './globals'
import { descriptionElementFromHash } from './helpers'

if (isPreview && isEmbedded) {
  const previewing = descriptionElementFromHash(true)

  if (previewing) {
    document.body.classList.add('preview')
    document.getElementById('content').replaceChildren(...previewing.childNodes)

    // Make links open in parent.
    const links = document.getElementsByTagName('a:not([target=_blank]')
    for (const element of links) {
      element.setAttribute('target', '_parent')
    }

    window.scrollTo(0, 0)
    // Stop iframe scrolling affecting parent by setting body position to fixed.
    document.body.style.position = 'fixed'
    // Defer preview message until all other scripts have run.
    setTimeout(sendPreviewInfoToParent)
    window.addEventListener('resize', sendPreviewInfoToParent)
  }
}

function sendPreviewInfoToParent () {
  const message = {
    type: 'preview',
    contentHeight: document.getElementById('content').parentElement.offsetHeight
  }
  window.parent.postMessage(message, '*')
}
