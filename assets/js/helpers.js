// Helpers
// =======

// Escape a string for use in a regular expression
exports.escapeText = function (text) {
  return text.replace(/[\-\[\]{}()*+?.,\\\^$|#\s]/g, '\\$&')
}
