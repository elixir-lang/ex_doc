'use strict'

// Dependencies
// ------------

var $ = require('jquery')
var hljs = require('highlightjs/highlight.pack')

var events = require('./events')

$(function () {
  // Setup Highlight.js
  hljs.configure({
    tabReplace: '    ', // 4 spaces
    languages: []       // disable auto-detect
  })

  events.initialize()
  hljs.initHighlighting()
})
