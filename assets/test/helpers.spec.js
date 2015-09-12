import * as helpers from '../js/helpers'

describe('helpers', () => {
  describe('escapeText', () => {
    var escapeText = helpers.escapeText

    it('escapes -', function () {
      expect(escapeText('hello-world')).to.be.equal('hello\\-world')
    })
  })
})
