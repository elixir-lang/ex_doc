export default function (context, node, options) {
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
}
