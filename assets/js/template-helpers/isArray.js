export default function (entry, options) {
  if (Array.isArray(entry)) {
    return options.fn(this)
  } else {
    return options.inverse(this)
  }
}
