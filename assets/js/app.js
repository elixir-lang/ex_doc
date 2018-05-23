// Dependencies
// ------------

import $ from 'jquery'
import hljs from 'highlight.js/build/highlight.pack'

import {initialize as initEvents} from './events'
import {initialize as initSidebar} from './sidebar'
import {initialize as initNightMode} from './night'
import {initialize as initMakeup} from './makeup'

window.$ = $

$(() => {
  // Setup Highlight.js
  hljs.configure({
    tabReplace: '    ', // 4 spaces
    languages: []       // disable auto-detect
  })

  initNightMode()
  initSidebar()
  initEvents()
  initMakeup()
  hljs.initHighlighting()
})
