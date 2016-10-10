export default function (context, nodeGroup, options) {
  var group = nodeGroup || ''
  if (context.group !== group) {
    context.group = group
    return options.fn(this)
  }
}
