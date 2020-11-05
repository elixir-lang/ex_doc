import { escapeRegexModifiers, findSidebarCategory } from '../js/helpers'

describe('helpers', () => {
  describe('escapeRegexModifiers', () => {
    it('escapes -', () => {
      expect(escapeRegexModifiers('hello-world')).to.be.equal('hello\\-world')
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

      expect(findSidebarCategory(nodes, 'world')).to.be.eql('functions')
      expect(findSidebarCategory(nodes, 'something')).to.be.eql(null)
    })
  })
})
