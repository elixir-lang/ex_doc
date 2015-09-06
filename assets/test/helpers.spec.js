var helpers = require('../js/helpers')

describe('helpers', function () {
  describe('escapeText', function () {
    var escapeText = helpers.escapeText

    it('escapes -', function () {
      expect(escapeText('hello-world')).to.be.equal('hello\\-world')
    })
  })
})
