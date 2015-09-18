export default function (node, options) {
  if (node.types || node.functions || node.macros || node.callbacks) {
    return options.fn(this)
  }
}
