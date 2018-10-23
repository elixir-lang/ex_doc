export default function (context, nestedContext, options) {
  // context.nestedContext is also reset each time a new group
  // is encountered (the value is reset within the #newGroup
  // block helper)
  if (context.nestedContext !== nestedContext) {
    context.nestedContext = nestedContext
    return options.fn(this)
  }
}
