import { shouldIgnoreVisit } from '../js/swup'

describe('shouldIgnoreVisit', () => {
  describe('same-page detection', () => {
    it('ignores exact same path', () => {
      expect(shouldIgnoreVisit('/doc/apps/stdlib/gen_server.html', '/doc/apps/stdlib/gen_server.html')).toBe(true)
    })

    it('ignores same path with hash', () => {
      expect(shouldIgnoreVisit('/doc/apps/stdlib/gen_server.html#start_link/3', '/doc/apps/stdlib/gen_server.html')).toBe(true)
    })

    it('ignores extensionless URL matching .html page', () => {
      expect(shouldIgnoreVisit('/doc/apps/stdlib/gen_server', '/doc/apps/stdlib/gen_server.html')).toBe(true)
    })

    it('ignores .html URL matching extensionless page', () => {
      expect(shouldIgnoreVisit('/doc/apps/stdlib/gen_server.html', '/doc/apps/stdlib/gen_server')).toBe(true)
    })
  })

  describe('same-directory links (use swup)', () => {
    it('allows .html links in same directory', () => {
      expect(shouldIgnoreVisit('/doc/apps/stdlib/lists.html', '/doc/apps/stdlib/gen_server.html')).toBe(false)
    })

    it('allows extensionless links in same directory', () => {
      expect(shouldIgnoreVisit('/doc/apps/stdlib/lists', '/doc/apps/stdlib/gen_server')).toBe(false)
    })

    it('allows extensionless link from .html page', () => {
      expect(shouldIgnoreVisit('/doc/apps/stdlib/lists', '/doc/apps/stdlib/gen_server.html')).toBe(false)
    })

    it('allows .html link from extensionless page', () => {
      expect(shouldIgnoreVisit('/doc/apps/stdlib/lists.html', '/doc/apps/stdlib/gen_server')).toBe(false)
    })
  })

  describe('cross-directory links (full reload)', () => {
    it('ignores cross-app links', () => {
      expect(shouldIgnoreVisit('/doc/apps/kernel/file.html', '/doc/apps/stdlib/gen_server.html')).toBe(true)
    })

    it('ignores cross-app extensionless links', () => {
      expect(shouldIgnoreVisit('/doc/apps/kernel/file', '/doc/apps/stdlib/gen_server')).toBe(true)
    })

    it('ignores links to system docs from app docs', () => {
      expect(shouldIgnoreVisit('/doc/system/design_principles', '/doc/apps/stdlib/gen_server')).toBe(true)
    })
  })
})
