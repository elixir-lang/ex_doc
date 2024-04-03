import { getLocationHash, descriptionElementFromHash } from './helpers'


export function initialize () {
  const previewing = getPreviewing()
  if (previewing) {
    preview(previewing)
  }
}

function preview (previewing) {
  cleanUpPreviewing(previewing)
  replaceContents(previewing)
  removeNonPreviewContents()
  makeLinksOpenInParent()
  scrollToTop()
}

function makeLinksOpenInParent () {
  const links = document.getElementsByTagName('a')
  for (let element of links) {
    console.log(element)
    if(element.getAttribute('target') !== '_blank') {
      element.setAttribute('target', '_parent')
    }
  }
}

function cleanUpPreviewing(previewing) {
  const detailHeader = previewing.querySelector('.detail-header')
  if (detailHeader) {
    detailHeader.classList.add('detail-header-preview')
  }
}

function scrollToTop() {
  window.scrollTo(0, 0)
}

function removeNonPreviewContents () {
  document.getElementById('sidebar').remove()
  document.getElementById('sidebar-menu').remove()
  document.getElementById('background-layer').remove()
  const bottomActions = document.getElementById('bottom-actions')
  bottomActions && bottomActions.remove()
  const footer = document.getElementById('footer')
  footer && footer.remove()
  Array.from(document.getElementsByTagName('a'))
    .filter((element) => element.getAttribute('href').startsWith('#'))
    .forEach(element => element.remove())
}

function replaceContents (previewing) {
  const contentContainer = document.getElementById('main')
  contentContainer.classList.add("content-preview")
  const content = document.getElementById('content')
  content.innerHTML = previewing.innerHTML
}

function getPreviewing () {
  const params = new URLSearchParams(window.location.search)
  if (params.has('preview')) {
    return descriptionElementFromHash(getLocationHash(), true)
  }
}
