import * as helpers from '../js/helpers'

describe('helpers', () => {
  describe('escapeText', () => {
    var escapeText = helpers.escapeText

    it('escapes -', function () {
      expect(escapeText('hello-world')).to.be.equal('hello\\-world')
    })
  })

  describe('findSidebarCategory', () => {
    it('finds the correct category', () => {
      const nodes = [{
        nodeGroups: [
          {key: 'callbacks', nodes: [{anchor: 'hello'}]},
          {key: 'functions', nodes: [{anchor: 'world'}]}
        ]
      }, {
        nodeGroups: [
          {key: 'callbacks', nodes: [{anchor: 'one'}]},
          {key: 'examples', nodes: [{anchor: 'two'}]}
        ]
      }]

      expect(helpers.findSidebarCategory(nodes, 'world')).to.be.eql('functions')
      expect(helpers.findSidebarCategory(nodes, 'something')).to.be.eql(undefined)
    })
  })
})
