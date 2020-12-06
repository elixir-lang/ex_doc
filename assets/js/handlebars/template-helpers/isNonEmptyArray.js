export default function (entry, options) {
  if (Array.isArray(entry) && entry.length > 0) {
    return options.fn(this)
  } else {
    return options.inverse(this)
  }
}
