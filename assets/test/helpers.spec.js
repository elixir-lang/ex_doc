import { escapeRegexModifiers } from '../js/helpers'

describe('helpers', () => {
  describe('escapeRegexModifiers', () => {
    it('escapes -', () => {
      expect(escapeRegexModifiers('hello-world')).to.be.equal('hello\\-world')
    })
  })
})
