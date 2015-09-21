export default function (nodeId, options) {
  var currentPath = window.location.pathname.split('/')
  nodeId = nodeId + '.html'

  if (nodeId === currentPath[currentPath.length - 1]) {
    return options.inverse(this)
  } else {
    return options.fn(this)
  }
}
