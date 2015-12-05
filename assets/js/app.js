// Dependencies
// ------------

import $ from 'jquery'
import hljs from 'highlight.js/build/highlight.pack'

import {initialize as initEvents} from './events'
import {initialize as initSidebar} from './sidebar'

window.$ = $
window.sidebarNodes = {}

$(() => {
  // Setup Highlight.js
  hljs.configure({
    tabReplace: '    ', // 4 spaces
    languages: []       // disable auto-detect
  })

  $.getJSON('dist/sidebar_items.json', function (data) {
    window.sidebarNodes = data
    initSidebar()
    initEvents()
  })
  hljs.initHighlighting()
})
