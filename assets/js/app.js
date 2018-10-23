// Dependencies
// ------------

import $ from 'jquery'
import Handlebars from 'handlebars/runtime'

import hljs from 'highlight.js/lib/highlight'
import bash from 'highlight.js/lib/languages/bash'
import css from 'highlight.js/lib/languages/css'
import diff from 'highlight.js/lib/languages/diff'
import erlang from 'highlight.js/lib/languages/erlang'
import erlangRepl from 'highlight.js/lib/languages/erlang-repl'
import http from 'highlight.js/lib/languages/http'
import javascript from 'highlight.js/lib/languages/javascript'
import json from 'highlight.js/lib/languages/json'
import markdown from 'highlight.js/lib/languages/markdown'
import sql from 'highlight.js/lib/languages/sql'
import xml from 'highlight.js/lib/languages/xml'

import hasContent from './template-helpers/hasContent'
import isArray from './template-helpers/isArray'
import isLocal from './template-helpers/isLocal'
import newGroup from './template-helpers/newGroup'
import nestingContextChanged from './template-helpers/nestingContextChanged'
import showSummary from './template-helpers/showSummary'

import {initialize as initEvents} from './events'
import {initialize as initSidebar} from './sidebar'
import {initialize as initNightMode} from './night'
import {initialize as initMakeup} from './makeup'

window.$ = $

$(() => {
  // Set up Handlebars.js
  Handlebars.registerHelper('hasContent', hasContent)
  Handlebars.registerHelper('isArray', isArray)
  Handlebars.registerHelper('isLocal', isLocal)
  Handlebars.registerHelper('newGroup', newGroup)
  Handlebars.registerHelper('nestingContextChanged', nestingContextChanged)
  Handlebars.registerHelper('showSummary', showSummary)

  // Set up Highlight.js
  hljs.configure({
    tabReplace: '    ', // 4 spaces
    languages: [] // disable auto-detect
  })
  hljs.registerLanguage('bash', bash)
  hljs.registerLanguage('css', css)
  hljs.registerLanguage('diff', diff)
  hljs.registerLanguage('erlang', erlang)
  hljs.registerLanguage('erlang-repl', erlangRepl)
  hljs.registerLanguage('http', http)
  hljs.registerLanguage('javascript', javascript)
  hljs.registerLanguage('json', json)
  hljs.registerLanguage('markdown', markdown)
  hljs.registerLanguage('sql', sql)
  hljs.registerLanguage('xml', xml)

  initNightMode()
  initSidebar()
  initEvents()
  initMakeup()
  hljs.initHighlighting()
})
