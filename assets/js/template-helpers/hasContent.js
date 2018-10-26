export default function (node, options) {
  if (hasItems(node.headers)) {
    return options.fn(this)
  }

  if (node.nodeGroups) {
    for (let {nodes} of node.nodeGroups) {
      if (hasItems(nodes)) {
        return options.fn(this)
      }
    }
  }

  return options.inverse(this)
}

function hasItems (items) {
  return Array.isArray(items) && (items.length > 0)
}
