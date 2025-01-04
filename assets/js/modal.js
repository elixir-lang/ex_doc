import { qs } from './helpers'
import modalLayoutHtml from './handlebars/templates/modal-layout.html'

const FOCUSABLE_SELECTOR = 'button:not([disabled]), [href], input:not([disabled]), select:not([disabled]), textarea:not([disabled]), [tabindex]:not([tabindex="-1"])'

// State

/** @type {HTMLDivElement | null} */
let modal = null
/** @type {HTMLElement | null} */
let prevFocus = null
/** @type {HTMLElement | null} */
let lastFocus = null
let ignoreFocusChanges = false

/**
 * Adds the modal to DOM, initially it's hidden.
 */
function renderModal () {
  if (modal) return

  document.body.insertAdjacentHTML('beforeend', modalLayoutHtml)
  modal = qs('.modal')

  modal.addEventListener('keydown', event => {
    if (event.key === 'Escape') {
      closeModal()
    }
  })

  modal.querySelector('.modal-close').addEventListener('click', closeModal)

  modal.addEventListener('click', event => {
    // if we clicked on the modal overlay/parent but not the modal content
    if (event.target === modal) {
      closeModal()
    }
  })
}

/**
 * Trap focus in modal
 * Only called on open modals
 */
function handleFocus (event) {
  if (ignoreFocusChanges) return

  if (modal.contains(event.target)) {
    lastFocus = event.target
  } else {
    ignoreFocusChanges = true
    const focusable = modal.querySelector(FOCUSABLE_SELECTOR)
    if (lastFocus === focusable[0]) {
      // Focus last
      focusable[focusable.length - 1].focus()
    } else {
      // Focus first
      focusable[0].focus()
    }
    ignoreFocusChanges = false
    lastFocus = document.activeElement
  }
}

/**
 * Shows modal with the given content.
 *
 * @param {{ title: string, body: string }} attrs
 */
export function openModal ({ title, body }) {
  renderModal()
  prevFocus = document.activeElement
  document.addEventListener('focus', handleFocus, true)

  modal.querySelector('.modal-title').innerHTML = title
  modal.querySelector('.modal-body').innerHTML = body

  modal.classList.add('shown')
  modal.focus()
}

/**
 * Closes the modal.
 */
export function closeModal () {
  modal?.classList.remove('shown')

  document.removeEventListener('focus', handleFocus, true)
  prevFocus?.focus()
  prevFocus = null
}

/**
 * Checks whether a modal is open.
 */
export function isModalOpen () {
  return Boolean(modal?.classList.contains('shown'))
}
