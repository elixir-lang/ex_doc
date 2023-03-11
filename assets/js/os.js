export function initialize () {
  const appleDeviceExpr = /(Macintosh|iPhone|iPad|iPod)/

  const osClass = appleDeviceExpr.test(window.navigator.userAgent) ? 'apple-os' : 'non-apple-os'
  document.documentElement.classList.add(osClass)
}
