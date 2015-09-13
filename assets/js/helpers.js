// Helpers
// =======

// Dependencies
// ------------

import $ from 'jquery'
import cssesc from 'cssesc'
import find from 'lodash.find'
import findKey from 'lodash.findKey'

// Escape a string for use in a regular expression
export function escapeText (text) {
  return text.replace(/[\-\[\]{}()*+?.,\\\^$|#\s]/g, '\\$&')
}

// Scroll to a given element in the page
export function scrollTo (target, elem, done) {
  return $(target)
    .animate({
      scrollTop: $(target).scrollTop() + $(elem).offset().top
    }, 300, done)
}

export function saveFind (selector) {
  return $(`#${cssesc(selector.replace(/^#/, ''), {isIdentifier: true})}`)
}

export function getModuleType () {
  let type = $('.content h1 small').text()

  if (type && (type === 'exception' || type === 'protocol')) {
    type = type + 's' // pluralize 'exception' or 'protocol'
  } else {
    type = 'modules'
  }

  return type
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
