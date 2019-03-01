// Dependencies
// ------------

import $ from 'jquery'
import autocompleteResultsTemplate from './templates/autocomplete-results.handlebars'

// Constants
// ---------

var AUTOCOMPLETE = $('.autocomplete')

// TODO: Clenaup + extract
// TODO: Add "_" and other chars allowed in elixir names
function autocompleteSanitize(text) {
  return text.replace(/[^A-Za-z0-9.]+/g, '')
}

function updateSuggestions(term) {
  var results = contentsJSON
    .filter(function (item) {
      return item.title.toLocaleLowerCase().indexOf(term.toLowerCase()) > -1
    })
    .slice(0, 5)
    .map(function (item) {
      var sanitizedTerm = autocompleteSanitize(term)
      var regexp = new RegExp(`(${ sanitizedTerm })`, 'i')
      var title = item.title.replace(regexp, '<strong class="autocomplete-foundFragment">$1</strong>')

      var resultItem = {
        title: title,
        doc: item.doc
      }
      return resultItem
    })

  var template = autocompleteResultsTemplate({
    results: results,
    term: term
  });
  AUTOCOMPLETE.html(template)
}

function hideAutocomplete () {
  AUTOCOMPLETE.hide()
}

function showAutocomplete () {
  AUTOCOMPLETE.show()
}

function updateAutocomplete (searchTerm) {
  if (!searchTerm) {
    hideAutocomplete()
  } else {
    showAutocomplete()
    updateSuggestions(searchTerm)
  }
}

function autocompleteMoveSelection(direction) {
  var currentlySelectedElement = $('.autocomplete-result.selected')
  var indexToSelect = -1
  if (currentlySelectedElement.length) {
    indexToSelect = parseInt(currentlySelectedElement.attr('data-index')) + direction
  }

  var elementToSelect = $(`.autocomplete-result[data-index="${indexToSelect}"]`)

  if (!elementToSelect.length) {
    elementToSelect = $('.autocomplete-result:first')
  }

  $('.autocomplete-result').each(function () {
    $(this).toggleClass('selected', $(this).is(elementToSelect))
  });
}

function updateSuggestions(term) {
  var results = contentsJSON
    .filter(function (item) {
      return item.title.toLocaleLowerCase().indexOf(term.toLowerCase()) > -1
    })
    .slice(0, 5)
    .map(function (item) {
      var sanitizedTerm = autocompleteSanitize(term)
      var regexp = new RegExp(`(${ sanitizedTerm })`, 'i')
      var title = item.title.replace(regexp, '<strong class="autocomplete-foundFragment">$1</strong>')

      var resultItem = {
        title: title,
        doc: item.doc
      }
      return resultItem
    })

  var template = autocompleteResultsTemplate({
    results: results,
    term: term
  });
  AUTOCOMPLETE.html(template)
}

// Public Methods
// --------------

export { updateAutocomplete }

export function initialize () {

}
