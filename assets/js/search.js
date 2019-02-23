/* globals contentsJSON */

// Search
// ======

// Dependencies
// ------------

import $ from 'jquery'
import lunr from 'lunr'
import resultsTemplate from './templates/search-results.handlebars'

// Local Variables
// ---------------

const $search = $('#search')
const $input = $('.sidebar-search input')

// Local Methods
// -------------

function fillResults (results, value) {
  var contents = contentsJSON
  return results.map(function (element) {
    var item = contents.find(i => i.ref === element.ref)
    var metadata = element.matchData.metadata
    item.metadata = metadata
    item.excerpts = getExcerpts(item, metadata, value)

    return item
  })
}

function getExcerpts (item, metadata, value) {
  var terms = value.split(' ')
  var excerpts = []
  var nchars = 80
  terms = Object.keys(metadata)
  terms.forEach(function (term) {
    if ('doc' in metadata[term]) {
      metadata[term].doc.position.forEach(function (matchPos) {
        var startPos = matchPos[0] - nchars > 0 ? matchPos[0] - nchars : 0
        var endPos = matchPos[0] + matchPos[1] + nchars > item.doc.length ?
                     item.doc.length : matchPos[0] + matchPos[1] + nchars
        var excerpt =
          (startPos > 0 ? '...' : '') +
          item.doc.slice(startPos, matchPos[0]) +
          '<em>' +
          item.doc.slice(matchPos[0], matchPos[0] + matchPos[1]) +
          '</em> ' +
          item.doc.slice(matchPos[0] + matchPos[1], endPos) +
          (endPos < item.doc.length ? '...' : '')
        excerpts.push(excerpt)
      })
    }
  })
  if (excerpts.length == 0) {
    excerpts.push(item.doc.slice(0, nchars * 2))
  }
  return excerpts.slice(0, 1)
}

export function search (value) {
  if (value.replace(/\s/, '') !== '') {
    var idx = getIndex()
    var results = idx.search(value)
    results = fillResults(results, value)
    var resultsHtml = resultsTemplate({
      value: value,
      results: results,
      empty: results.length === 0
    })

    $input.val(value)
    $search.html(resultsHtml)
  }
}

function getIndex () {
  if (sessionStorage.getItem('idx') == null) {
    createIndex()
  }
  return lunr.Index.load(JSON.parse(sessionStorage.getItem('idx')))
}

export function createIndex () {
  var idx = lunr(function () {
    this.ref('ref')
    this.field('text')
    this.field('title')
    this.field('module')
    this.field('type')
    this.field('doc')
    this.metadataWhitelist = ['position']
    contentsJSON.forEach(function (doc) {
      this.add(doc)
    }, this)
  })
  sessionStorage.setItem('idx', JSON.stringify(idx))
}
