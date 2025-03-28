import * as Handlebars from 'handlebars/runtime'

Handlebars.registerHelper('isArray', function (entry, options) {
  if (Array.isArray(entry)) {
    return options.fn(this)
  } else {
    return options.inverse(this)
  }
})

Handlebars.registerHelper('isNonEmptyArray', function (entry, options) {
  if (Array.isArray(entry) && entry.length > 0) {
    return options.fn(this)
  } else {
    return options.inverse(this)
  }
})
