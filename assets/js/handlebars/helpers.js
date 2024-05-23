Handlebars.registerHelper('groupChanged', function (context, nodeGroup, options) {
  const group = nodeGroup || ''
  if (context.group !== group) {
    // reset the nesting context for the #nestingChanged block helper
    delete context.nestedContext
    context.group = group
    return options.fn(this)
  }
})

Handlebars.registerHelper('nestingChanged', function (context, node, options) {
  // context.nestedContext is also reset each time a new group
  // is encountered (the value is reset within the #groupChanged
  // block helper)
  if (node.nested_context && node.nested_context !== context.nestedContext) {
    context.nestedContext = node.nested_context

    if (context.lastModuleSeenInGroup !== node.nested_context) {
      return options.fn(this)
    }
  } else {
    // track the most recently seen module
    // prevents emitting a duplicate entry for nesting when
    // the nesting prefix matches an existing module
    context.lastModuleSeenInGroup = node.title
  }
})

Handlebars.registerHelper('showSections', function (node, options) {
  if (node.sections.length > 0) {
    return options.fn(this)
  }
})

Handlebars.registerHelper('showSummary', function (node, options) {
  if (node.nodeGroups) {
    return options.fn(this)
  }
})

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

Handlebars.registerHelper('isEmptyArray', function (entry, options) {
  if (Array.isArray(entry) && entry.length === 0) {
    return options.fn(this)
  } else {
    return options.inverse(this)
  }
})

Handlebars.registerHelper('isLocal', function (nodeId, options) {
  const pathSuffix = window.location.pathname.split('/').pop()

  if (pathSuffix === nodeId + '.html' || pathSuffix === nodeId) {
    return options.fn(this)
  } else {
    return options.inverse(this)
  }
})
