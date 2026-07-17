import { escapeRegexModifiers } from '../js/helpers'

describe('helpers', () => {
  describe('escapeRegexModifiers', () => {
    it('escapes -', () => {
      expect(escapeRegexModifiers('hello-world')).toBe('hello\\-world')
    })
  })
})
