import modalLayoutTemplate from './handlebars/templates/modal-layout.handlebars'
import { qs } from './helpers'

const MODAL_SELECTOR = '#modal'
const MODAL_CLOSE_BUTTON_SELECTOR = '#modal .modal-close'
const MODAL_TITLE_SELECTOR = '#modal .modal-title'
const MODAL_BODY_SELECTOR = '#modal .modal-body'
let prevFocus = null

/**
 * Initializes modal layout.
 */
export function initialize () {
  renderModal()
}

/**
 * Adds the modal to DOM, initially it's hidden.
 */
function renderModal () {
  const modalLayoutHtml = modalLayoutTemplate()
  document.body.insertAdjacentHTML('beforeend', modalLayoutHtml)

  qs(MODAL_SELECTOR).addEventListener('keydown', event => {
    if (event.key === 'Escape') {
      closeModal()
    }
  })

  qs(MODAL_CLOSE_BUTTON_SELECTOR).addEventListener('click', event => {
    closeModal()
  })
}

/**
 * Trap focus in modal
 * Only called on open modals
 */
function trapFocus (e) {
  if (e.key !== 'Tab') return;

  // trap focus
  const nodes = qs(MODAL_SELECTOR).querySelectorAll('*')
  const tabbable = Array.from(nodes).filter(n => n.tabIndex >= 0)

  let index = tabbable.indexOf(document.activeElement)
  if (index === -1 && e.shiftKey) index = 0

  console.log(index)

  index += tabbable.length + (e.shiftKey ? -1 : 1)
  index %= tabbable.length

  console.log(index, tabbable)

  tabbable[index].focus()
  e.preventDefault()
}

/**
 * Shows modal with the given content.
 *
 * @param {{ title: String, body: String }} attrs
 */
export function openModal ({ title, body }) {
  prevFocus = typeof document !== 'undefined' && document.activeElement
  window.addEventListener("keydown", trapFocus)

  qs(MODAL_TITLE_SELECTOR).innerHTML = title
  qs(MODAL_BODY_SELECTOR).innerHTML = body

  qs(MODAL_SELECTOR).classList.add('shown')
  qs(MODAL_SELECTOR).focus()
}

/**
 * Closes the modal.
 */
export function closeModal () {
  qs(MODAL_SELECTOR).classList.remove('shown')

  window.removeEventListener("keydown", trapFocus)
  console.log(prevFocus)
  prevFocus && prevFocus.focus()
}

/**
 * Checks whether a modal is open.
 */
export function isModalOpen () {
  return qs(MODAL_SELECTOR).classList.contains('shown')
}
