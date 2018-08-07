export default function (node, options) {
  if (node.nodeGroups) {
    return options.fn(this)
  }
}
