// Helpers
// =======

// Dependencies
// ------------

import find from 'lodash.find'

export const qs = document.querySelector.bind(document)

export const qsAll = document.querySelectorAll.bind(document)

// Escape a string for use in a regular expression
export function escapeText (text) {
  return text.replace(/[\-\[\]{}()*+?.,\\\^$|#\s]/g, '\\$&')
}

// Escape HTML entities
export function escapeHtmlEntities (text) {
  return String(text)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
}

export function getModuleType () {
  return qs('body').dataset.type
}

// Find out if the anchor belongs to either
// Types, Functions, Macros or Callbacks
export function findSidebarCategory (items, query) {
  if (!items) return

  for (let item of items) {
    const res = find(item.nodeGroups, ({nodes}) => {
      return find(nodes, ({anchor}) => anchor === query)
    })

    if (res) return res.key
  }
}

export function getLocationHash () {
  return window.location.hash.replace(/^#/, '')
}

export function checkUrlExists (url) {
  return fetch(url)
    .then(response => response.ok)
    .catch(() => false)
}

export function onDocumentReady (callback) {
  if (document.readyState !== 'loading') {
    callback()
  } else {
    document.addEventListener('DOMContentLoaded', callback)
  }
}
