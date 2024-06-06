import { isAppleOS } from './helpers'

export function initialize () {
  const osClass = isAppleOS() ? 'apple-os' : 'non-apple-os'
  document.documentElement.classList.add(osClass)
}
