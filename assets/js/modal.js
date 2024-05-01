import { qs } from './helpers'

const MODAL_SELECTOR = '#modal'
const MODAL_CLOSE_BUTTON_SELECTOR = '#modal .modal-close'
const MODAL_TITLE_SELECTOR = '#modal .modal-title'
const MODAL_BODY_SELECTOR = '#modal .modal-body'
const FOCUSABLE_SELECTOR = 'button:not([disabled]), [href], input:not([disabled]), select:not([disabled]), textarea:not([disabled]), [tabindex]:not([tabindex="-1"])'
const state = {
  prevFocus: null,
  lastFocus: null,
  ignoreFocusChanges: false
}

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
  const modalLayoutHtml = Handlebars.templates['modal-layout']()
  document.body.insertAdjacentHTML('beforeend', modalLayoutHtml)

  qs(MODAL_SELECTOR).addEventListener('keydown', event => {
    if (event.key === 'Escape') {
      closeModal()
    }
  })

  qs(MODAL_CLOSE_BUTTON_SELECTOR).addEventListener('click', event => {
    closeModal()
  })

  qs(MODAL_SELECTOR).addEventListener('click', event => {
    const classList = event.target.classList
    // if we clicked on the modal overlay/parent but not the modal content
    if (classList.contains('modal') && classList.contains('shown') && classList.length === 2) {
      closeModal()
    }
  })
}

/**
 * Trap focus in modal
 * Only called on open modals
 */
function trapFocus (event) {
  if (state.ignoreFocusChanges) return
  const modal = qs(MODAL_SELECTOR)
  if (modal.contains(event.target)) {
    state.lastFocus = event.target
  } else {
    state.ignoreFocusChanges = true
    const firstFocusable = firstFocusableDescendant(modal)
    if (state.lastFocus === firstFocusable) {
      lastFocusableDescendant(modal).focus()
    } else {
      firstFocusable.focus()
    }
    state.ignoreFocusChanges = false
    state.lastFocus = document.activeElement
  }
}

function firstFocusableDescendant (element) {
  return element.querySelector(FOCUSABLE_SELECTOR)
}

function lastFocusableDescendant (element) {
  const elements = element.querySelectorAll(FOCUSABLE_SELECTOR)
  return elements[elements.length - 1]
}

/**
 * Shows modal with the given content.
 *
 * @param {{ title: String, body: String }} attrs
 */
export function openModal ({ title, body }) {
  state.prevFocus = document.activeElement
  document.addEventListener('focus', trapFocus, true)

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

  document.addEventListener('focus', trapFocus, true)
  state.prevFocus && state.prevFocus.focus()
  state.prevFocus = null
}

/**
 * Checks whether a modal is open.
 */
export function isModalOpen () {
  return qs(MODAL_SELECTOR).classList.contains('shown')
}
