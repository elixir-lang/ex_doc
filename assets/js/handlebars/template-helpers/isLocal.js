export default function (nodeId, options) {
  const pathSuffix = window.location.pathname.split('/').pop().replace(/-.*/, '.html')
  const nodePage = nodeId + '.html'

  if (nodePage === pathSuffix) {
    return options.fn(this)
  } else {
    return options.inverse(this)
  }
}
