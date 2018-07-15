export default function (node, options) {
  if (node.types || node.functions || node.guards || node.callbacks) {
    return options.fn(this)
  }
}
