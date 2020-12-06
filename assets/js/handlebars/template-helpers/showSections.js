export default function (node, options) {
  if (node.sections.length > 0) {
    return options.fn(this)
  }
}
