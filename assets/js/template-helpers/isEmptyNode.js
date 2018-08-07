export default function (node, options) {
  if (hasItems(node.headers)) {
    return options.inverse(this)
  }

  if (node.nodeGroups) {
    for (let {nodes} of node.nodeGroups) {
      if (hasItems(nodes)) {
        return options.inverse(this)
      }
    }
  }

  return options.fn(this)
}

function hasItems(items) {
  return Array.isArray(items) && (items.length > 0)
}
