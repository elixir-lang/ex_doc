export default function (string, translations, options) {
  switch (string) {
    case 'callback':
      return translations.callback
    case 'Callbacks':
      return translations.callbacks
    case 'Contributing':
      return translations.contributing
    case 'extras':
      return translations.extras
    case 'function':
      return translations.function
    case 'Functions':
      return translations.functions
    case 'Go to':
      return translations.go_to
    case 'Keyboard Shortcuts':
      return translations.keyboard_shortcuts
    case 'License':
      return translations.license
    case 'Summary':
      return translations.summary
    case 'task':
      return translations.task
    case 'Top':
      return translations.top
    case 'Types':
      return translations.types
    case 'Search results for':
      return translations.search_results
    case 'Search the documentation':
      return translations.search_documentation
    default:
      return string
  }
};
