export default function (nodeId, options) {
  var currentPath = window.location.pathname.split('/')
  nodeId = nodeId + '.html'

  if (nodeId === currentPath[currentPath.length - 1]) {
    return options.fn(this)
  } else {
    return options.inverse(this)
  }
}
