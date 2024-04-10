import { getLocationHash, descriptionElementFromHash } from './helpers'

export function initialize () {
  const previewing = descriptionElementFromHash(getLocationHash(), true)

  if (previewing) {
    preview(previewing)
  }
}

function preview (previewing) {
  replaceContents(previewing)
  makeLinksOpenInParent()
  scrollToTop()
  sendPreviewInfoToParent()

  window.addEventListener('resize', event => {
    sendPreviewInfoToParent()
  })
}

function sendPreviewInfoToParent () {
  const maxHeight = document.body.scrollHeight
  const contentHeight = document.getElementById('content').parentElement.offsetHeight
  const message = {
    type: 'preview',
    maxHeight,
    contentHeight
  }
  window.parent.postMessage(message, '*')
}

function makeLinksOpenInParent () {
  const links = document.getElementsByTagName('a')
  for (const element of links) {
    if (element.getAttribute('target') !== '_blank') {
      element.setAttribute('target', '_parent')
    }
  }
}

function scrollToTop () {
  window.scrollTo(0, 0)
}

function replaceContents (previewing) {
  document.body.classList.add('preview')
  const content = document.getElementById('content')
  content.innerHTML = previewing.innerHTML
}
