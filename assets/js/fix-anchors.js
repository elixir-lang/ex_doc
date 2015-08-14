'use strict'

var $ = require('jquery')

// Because of the fixed header the top part of the
// anchor links is hidden when they are displayed
// this removes this problem.

var adjustAnchor = function () {
  var $anchor = $(':target')
  var fixedElementHeight = 100

  if ($anchor.length > 0) {
    window.scrollTo(0, $anchor.offset().top - fixedElementHeight)
  }
}

$(window).on('hashchange load', function () {
  adjustAnchor()
})
