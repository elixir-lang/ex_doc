export default function (node, options) {
  var nodeItems = [
    node.types,
    node.functions,
    node.macros,
    node.callbacks,
    node.headers
  ].filter(Array.isArray);

  if (flatten(nodeItems).length === 0){
    return options.fn(this);
  } else {
    return options.inverse(this);
  }
}

function flatten(array) {
  return array.reduce(function(a, b){ return a.concat(b);}, [])
}
