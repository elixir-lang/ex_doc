'use strict'

// Dependencies
// ------------

var $ = window.$ = require('jquery')
var hljs = require('highlightjs/highlight.pack')

var sidebar = require('./sidebar')
var events = require('./events')

require('./fix-anchors')

$(function () {
  // Setup Highlight.js
  hljs.configure({
    tabReplace: '    ', // 4 spaces
    languages: []       // disable auto-detect
  })

  sidebar.init()
  events.initialize()
  hljs.initHighlighting()
})
