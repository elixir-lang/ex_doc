export default function (node, options) {
  var nodeItems = [
    node.headers,
    node.types,
    node.functions,
    node.guards,
    node.callbacks
  ]

  for (var i = 0; i < nodeItems.length; i++) {
    if (Array.isArray(nodeItems[i]) && (nodeItems[i].length > 0)) {
      return options.inverse(this)
    }
  }

  return options.fn(this)
}
