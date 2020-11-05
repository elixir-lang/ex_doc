export default function (context, nodeGroup, options) {
  const group = nodeGroup || ''
  if (context.group !== group) {
    // reset the nesting context for the #nestingChanged block helper
    delete context.nestedContext
    context.group = group
    return options.fn(this)
  }
}
