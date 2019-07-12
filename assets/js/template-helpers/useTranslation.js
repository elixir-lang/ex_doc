export default function (string, translations, options) {
  switch (string) {
    case 'Bring up this help dialog':
      return translations.bring_dialog
    case 'callback':
      return translations.callback
    case 'Callbacks':
      return translations.callbacks
    case 'Contributing':
      return translations.contributing
    case 'extras':
      return translations.extras
    case 'Focus search bar':
      return translations.focus_search_bar
    case 'function':
      return translations.function
    case 'Functions':
      return translations.functions
    case 'Go to':
      return translations.go_to
    case 'Go to a HexDocs package':
      return translations.go_to_hex
    case 'Here are some tips when performing a full-text search:':
      return translations.not_found_search_hint
    case 'Keyboard Shortcuts':
      return translations.keyboard_shortcuts
    case 'License':
      return translations.license
    case 'macro':
      return translations.macro
    case 'module':
      return translations.module
    case 'Summary':
      return translations.summary
    case 'task':
      return translations.task
    case 'Toggle sidebar':
      return translations.toggle_sidebar
    case 'Toggle night mode':
      return translations.toggle_night_mode
    case 'Top':
      return translations.top
    case 'type':
      return translations.type
    case 'Types':
      return translations.types
    case 'Search results for':
      return translations.search_results
    case 'Search the documentation':
      return translations.search_documentation
    case 'Sorry, we couldn\'t find anything for':
      return translations.not_found_search
    case 'Multiple words (such as <code>foo bar</code>) are searched as <code>OR</code>':
      return translations.not_found_search_hint_1
    case 'Use <code>*</code> anywhere (such as <code>fo*</code>) as wildcard':
      return translations.not_found_search_hint_2
    case 'Use <code>+</code> before a word (such as <code>+foo</code>) to make its presence required':
      return translations.not_found_search_hint_3
    case 'Use <code>-</code> before a word (such as <code>-foo</code>) to make its absence required':
      return translations.not_found_search_hint_4
    case 'Use <code>WORD^NUMBER</code> (such as <code>foo^2</code>) to boost the given word':
      return translations.not_found_search_hint_5
    case 'Use <code>WORD~NUMBER</code> (such as <code>foo~2</code>) to do a search with edit distance on word':
      return translations.not_found_search_hint_6
    case 'To quickly go to a module, type, or function, use the autocompletion feature in the sidebar search.':
      return translations.not_found_search_hint_7
    default:
      return string
  }
};
