export default function (context, nodeGroup, options) {
  const group = nodeGroup || ''
  if (context.group !== group) {
    context.group = group
    return options.fn(this)
  }
}
