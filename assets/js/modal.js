import modalLayoutTemplate from './handlebars/templates/modal-layout.handlebars'
import { qs } from './helpers'

const MODAL_SELECTOR = '#modal'
const MODAL_CLOSE_BUTTON_SELECTOR = '#modal .modal-close'
const MODAL_TITLE_SELECTOR = '#modal .modal-title'
const MODAL_BODY_SELECTOR = '#modal .modal-body'

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
 * Shows modal with the given content.
 *
 * @param {{ title: String, body: String }} attrs
 */
export function openModal ({ title, body }) {
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
}

/**
 * Checks whether a modal is open.
 */
export function isModalOpen () {
  return qs(MODAL_SELECTOR).classList.contains('shown')
}
