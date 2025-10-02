import { debounce, el, qs, qsAll } from './helpers'
import { openModal } from './modal'
import quickSwitchModalBodyHtml from './handlebars/templates/quick-switch-modal-body.html'
import { isEmbedded } from './globals'

const HEX_DOCS_ENDPOINT = 'https://hexdocs.pm/%%'
const OTP_DOCS_ENDPOINT = 'https://www.erlang.org/doc/apps/%%'
const HEX_SEARCH_ENDPOINT = 'https://hex.pm/api/packages?search=name:%%*'
const QUICK_SWITCH_LINK_SELECTOR = '.display-quick-switch'
const QUICK_SWITCH_INPUT_SELECTOR = '#quick-switch-input'
const QUICK_SWITCH_RESULTS_SELECTOR = '#quick-switch-results'
const DEBOUNCE_KEYPRESS_TIMEOUT = 300
const NUMBER_OF_SUGGESTIONS = 9

const OTP_APPS = [
  'erts',
  'asn1',
  'common_test',
  'compiler',
  'crypto',
  'debugger',
  'dialyzer',
  'diameter',
  'edoc',
  'eldap',
  'erl_interface',
  'et',
  'eunit',
  'ftp',
  'inets',
  'jinterface',
  'kernel',
  'megaco',
  'mnesia',
  'observer',
  'odbc',
  'os_mon',
  'parsetools',
  'public_key',
  'reltool',
  'runtime_tools',
  'sasl',
  'snmp',
  'ssh',
  'ssl',
  'stdlib',
  'syntax_tools',
  'tftp',
  'tools',
  'wx',
  'xmerl'
]

// Core Elixir/OTP packages to include in the autocomplete results
const STATIC_SEARCH_RESULTS = [
  'elixir',
  'eex',
  'ex_unit',
  'hex',
  'iex',
  'logger',
  'mix'
].concat(OTP_APPS).map(name => ({ name }))

const MIN_SEARCH_LENGTH = 2

const state = {
  autocompleteResults: [],
  selectedIdx: null
}

/**
 * Initializes the quick switch modal.
 */

if (!isEmbedded) {
  window.addEventListener('exdoc:loaded', initialize)
}

function initialize () {
  qsAll(QUICK_SWITCH_LINK_SELECTOR).forEach(element => {
    element.addEventListener('click', openQuickSwitchModal)
  })
}

function handleKeyDown (event) {
  if (event.key === 'Enter') {
    const packageSlug = event.target.value

    quickSwitchToAppDocs(packageSlug)
    event.preventDefault()
  } else if (event.key === 'ArrowUp') {
    moveAutocompleteSelection(-1)
    event.preventDefault()
  } else if (event.key === 'ArrowDown') {
    moveAutocompleteSelection(1)
    event.preventDefault()
  }
}

function handleInput (event) {
  const packageSlug = event.target.value

  if (packageSlug.length < MIN_SEARCH_LENGTH) {
    const resultsContainer = qs(QUICK_SWITCH_RESULTS_SELECTOR)
    resultsContainer.innerHTML = ''
  } else {
    debouncedQueryForAutocomplete(packageSlug)
  }
}

/**
 * Opens the quick switch modal dialog.
 */
export function openQuickSwitchModal () {
  openModal({
    title: 'Go to package docs',
    body: quickSwitchModalBodyHtml
  })

  const quickSwitchInput = qs(QUICK_SWITCH_INPUT_SELECTOR)
  quickSwitchInput.focus()
  quickSwitchInput.addEventListener('keydown', handleKeyDown)
  quickSwitchInput.addEventListener('input', handleInput)

  state.autocompleteResults = []
  state.selectedIdx = null
}

/**
 * Navigate to application docs.
 *
 * If an autocomplete entry is selected, it will be used instead of the input text.
 *
 * @param {String} name The searched application name
 */
function quickSwitchToAppDocs (name) {
  if (state.selectedIdx === null) {
    navigateToAppDocs(name)
  } else {
    const selectedResult = state.autocompleteResults[state.selectedIdx]
    navigateToAppDocs(selectedResult.name)
  }
}

/**
 * Navigates to app docs.
 *
 * For Hex packages and Elixir apps go to hexdocs.pm. For OTP apps, erlang.org/doc.
 *
 * @param {String} app The application name to navigate to
 */
function navigateToAppDocs (app) {
  if (OTP_APPS.includes(app.toLowerCase())) {
    window.location = OTP_DOCS_ENDPOINT.replace('%%', app.toLowerCase())
  } else {
    window.location = HEX_DOCS_ENDPOINT.replace('%%', app.toLowerCase())
  }
}

const debouncedQueryForAutocomplete = debounce(queryForAutocomplete, DEBOUNCE_KEYPRESS_TIMEOUT)

/**
 * Queries the HexDocs API for autocomplete results for a given package name.
 *
 * @param {String} packageSlug The searched package name
 */
function queryForAutocomplete (packageSlug) {
  const url = HEX_SEARCH_ENDPOINT.replace('%%', packageSlug)
  fetch(url)
    .then(response => response.json())
    .then(payload => {
      if (Array.isArray(payload)) {
        state.autocompleteResults = resultsFromPayload(packageSlug, payload)
        state.selectedIdx = null
        // Only render results if the search string is still long enough
        const currentTerm = qs(QUICK_SWITCH_INPUT_SELECTOR).value
        if (currentTerm.length >= MIN_SEARCH_LENGTH) {
          renderResults(state.autocompleteResults)
        }
      }
    })
}

function renderResults (results) {
  qs(QUICK_SWITCH_RESULTS_SELECTOR).replaceChildren(...results.map(({name}, index) => {
    const resultEl = el('div', {class: 'quick-switch-result', 'data-index': index}, [name])
    resultEl.addEventListener('click', () => navigateToAppDocs(name))
    return resultEl
  }))
}

/**
 * Extracts the first `NUMBER_OF_SUGGESTIONS` results from the payload response
 * and the hardcoded, available packages in `STATIC_SEARCH_RESULT`.
 * This also filters out packages that do not have their docs published on HexDocs.
 *
 * @param {String} packageSlug The searched package name
 * @param {Array} payload The payload returned by the search request
 */
function resultsFromPayload (packageSlug, payload) {
  return STATIC_SEARCH_RESULTS
    .concat(payload)
    .filter(result => result.name.toLowerCase().includes(packageSlug.toLowerCase()))
    .filter(result => result.releases === undefined || result.releases[0].has_docs === true)
    .slice(0, NUMBER_OF_SUGGESTIONS)
}

/**
 * Moves the autocomplete selection up or down.
 *
 * @param {Number} offset How much to move down (or up) the list.
 */
function moveAutocompleteSelection (offset) {
  state.selectedIdx = newAutocompleteIndex(offset)

  const selectedElement = qs('.quick-switch-result.selected')
  const elementToSelect = qs(`.quick-switch-result[data-index="${state.selectedIdx}"]`)

  if (selectedElement) {
    selectedElement.classList.remove('selected')
  }

  if (elementToSelect) {
    elementToSelect.classList.add('selected')
  }
}

function newAutocompleteIndex (offset) {
  const length = state.autocompleteResults.length

  if (state.selectedIdx === null) {
    if (offset >= 0) return 0
    if (offset < 0) return length - 1
  }

  const index = state.selectedIdx + offset
  return (index + length) % length
}
