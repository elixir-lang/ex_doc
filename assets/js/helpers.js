// Helpers
// =======

// Dependencies
// ------------

import $ from 'jquery'
import find from 'lodash.find'

// Escape a string for use in a regular expression
export function escapeText (text) {
  return text.replace(/[\-\[\]{}()*+?.,\\\^$|#\s]/g, '\\$&')
}

export function getModuleType () {
  return $('body').data('type')
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
