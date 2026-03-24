import { escapeRegexModifiers, getPageExtension } from '../js/helpers'

describe('helpers', () => {
  describe('escapeRegexModifiers', () => {
    it('escapes -', () => {
      expect(escapeRegexModifiers('hello-world')).toBe('hello\\-world')
    })
  })

  describe('getPageExtension', () => {
    it('returns .html when pathname ends with .html', () => {
      setPathname('/doc/apps/stdlib/gen_server.html')
      expect(getPageExtension()).toBe('.html')
    })

    it('returns empty string for extensionless doc page', () => {
      setPathname('/doc/apps/stdlib/gen_server')
      expect(getPageExtension()).toBe('')
    })

    it('returns .html for root path', () => {
      setPathname('/')
      expect(getPageExtension()).toBe('.html')
    })

    it('returns .html for empty last segment', () => {
      setPathname('/doc/apps/')
      expect(getPageExtension()).toBe('.html')
    })
  })
})

function setPathname (path) {
  delete window.location
  window.location = new URL('http://localhost' + path)
}
