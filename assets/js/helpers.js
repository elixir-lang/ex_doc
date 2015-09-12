// Helpers
// =======

// Dependencies
// ------------

import $ from 'jquery'

// Escape a string for use in a regular expression
export function escapeText (text) {
  return text.replace(/[\-\[\]{}()*+?.,\\\^$|#\s]/g, '\\$&')
}

// Scroll to a given element in the page
export function scrollTo (target, elem, done) {
  return $(target)
    .animate({
      scrollTop: $(elem).offset().top
    }, 300, done)
}
