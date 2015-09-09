// Helpers
// =======

// Dependencies
// ------------

var $ = require('jquery')

// Escape a string for use in a regular expression
exports.escapeText = function (text) {
  return text.replace(/[\-\[\]{}()*+?.,\\\^$|#\s]/g, '\\$&')
}

// Scroll to a given element in the page
exports.scrollTo = function (target, elem, done) {
  return $(target)
    .animate({
      scrollTop: $(elem).offset().top
    }, 300, done)
}
