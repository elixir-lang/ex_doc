// Helpers
// =======

// Dependencies
// ------------

import $ from 'jquery'
import find from 'lodash.find'
import findKey from 'lodash.findkey'

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
  for (let item of items) {
    const res = findKey(item, (value, key) => {
      const a = find(value, ({anchor}) => anchor === query)
      return a
    })

    if (res) return res
  }
}
