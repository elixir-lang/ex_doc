export default function (node, options) {
  if (hasPrefix(node) && !node.title_collapsed) {
    return options.fn(this)
  }

  return options.inverse(this)
}

function hasPrefix (node) {
  return node.title_prefix && node.title_prefix.length > 1;
}
