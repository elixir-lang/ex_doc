// Dependencies
// ------------

import $ from 'jquery'
import hljs from 'highlight.js/build/highlight.pack'

import {initialize as initEvents} from './events'
import {initialize as initSidebar} from './sidebar'

window.$ = $

$(() => {
  // Setup Highlight.js
  hljs.configure({
    tabReplace: '    ', // 4 spaces
    languages: []       // disable auto-detect
  })

  initSidebar()
  initEvents()
  hljs.initHighlighting()
})
