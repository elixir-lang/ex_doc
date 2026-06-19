import { el } from '../js/helpers'
import { LINK_SELECTOR, isWithinBuild } from '../js/swup'

const linkMatches = (href) => el('a', {href}).matches(LINK_SELECTOR)

describe('swup', () => {
  describe('LINK_SELECTOR', () => {
    it('matches local .html pages, with or without an anchor', () => {
      expect(linkMatches('Foo.html')).toBe(true)
      expect(linkMatches('Foo.Bar.html')).toBe(true)
      expect(linkMatches('file.html#section')).toBe(true)
      // Anchors carry slashes (name/arity); the selector must still match them.
      expect(linkMatches('file.html#list_dir/1')).toBe(true)
    })

    it('ignores non-HTML files (#2182), absolute and external links', () => {
      expect(linkMatches('ecto_erd.mmd')).toBe(false)
      expect(linkMatches('image.png')).toBe(false)
      expect(linkMatches('/Foo.html')).toBe(false)
      expect(linkMatches('http://example.com/Foo.html')).toBe(false)
      expect(linkMatches('https://example.com/Foo.html')).toBe(false)
    })

    it('does not match bare same-page anchors (scrolled natively, no SWUP)', () => {
      expect(linkMatches('#section')).toBe(false)
    })
  })

  describe('isWithinBuild', () => {
    it('is true for a same-folder link (bare filename)', () => {
      expect(isWithinBuild('Foo.html')).toBe(true)
      expect(isWithinBuild('file.html#section')).toBe(true)
      // A slash in the fragment (name/arity) does not leave the build.
      expect(isWithinBuild('file.html#list_dir/1')).toBe(true)
    })

    it('is false for a link into another folder/build', () => {
      expect(isWithinBuild('apps/kernel/file.html')).toBe(false)
      expect(isWithinBuild('../stdlib/lists.html#foo/1')).toBe(false)
    })
  })
})
