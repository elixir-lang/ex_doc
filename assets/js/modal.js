import modalLayoutTemplate from './handlebars/templates/modal-layout.handlebars'
import { qs } from './helpers'

const MODAL_SELECTOR = '#modal'
const MODAL_CLOSE_BUTTON_SELECTOR = '#modal .modal-close'
const MODAL_TITLE_SELECTOR = '#modal .modal-title'
const MODAL_BODY_SELECTOR = '#modal .modal-body'
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
function trapFocus (event) {
  if (state.ignoreFocusChanges) return
  var modal = qs(MODAL_SELECTOR)
  if (modal.contains(event.target)) {
    state.lastFocus = event.target
  } else {
    focusFirstDescendant(modal)
    if (state.lastFocus == document.activeElement) {
      focusLastDescendant(modal)
    }
    state.lastFocus = document.activeElement
  }
}

function focusFirstDescendant(element) {
  for (var i = 0; i < element.childNodes.length; i++) {
    var child = element.childNodes[i]
    if (attemptFocus(child) || focusFirstDescendant(child)) {
      return true
    }
  }
  return false
}

function focusLastDescendant(element) {
  for (var i = element.childNodes.length - 1; i >= 0; i--) {
    var child = element.childNodes[i]
    if (attemptFocus(child) || focusLastDescendant(child)) {
      return true
    }
  }
  return false
}

function attemptFocus (element) {
  if (!isFocusable(element)) return false

  state.ignoreFocusChanges = true
  try {
    element.focus()
  } catch (e) {
    // continue regardless of error
  }
  state.ignoreFocusChanges = false
  return document.activeElement === element
}

function isFocusable (element) {
  if (element.tabIndex < 0) return false

  if (element.disabled) return false

  switch (element.nodeName) {
    case 'A':
      return !!element.href && element.rel != 'ignore'
    case 'INPUT':
      return element.type != 'hidden'
    case 'BUTTON':
    case 'SELECT':
    case 'TEXTAREA':
      return true
    default:
      return false
  }
}

/**
 * Shows modal with the given content.
 *
 * @param {{ title: String, body: String }} attrs
 */
export function openModal ({ title, body }) {
  state.prevFocus = typeof document !== 'undefined' && document.activeElement
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
