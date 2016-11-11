import * as search from '../js/search'

describe('search', () => {
  describe('findIn', () => {
    it('returns a list of matches', () => {
      var nodes = [{id: 'hello world'}, {id: 'world'}]

      expect(search.findIn(nodes, 'hello')).to.be.eql([
        {id: 'hello world', match: '<em>hello</em> world'}
      ])
    })

    it('searches for function matches', () => {
      var nodes = [{
        id: 'hello world',
        functions: [
          {id: 'hello world', anchor: 'hello-world'}
        ]
      }, {
        id: 'world',
        functions: [
          {id: 'hello world', anchor: 'hello-world'}
        ]
      }, {
        id: 'world2',
        functions: [
          {id: 'world', anchor: 'world'}
        ]
      }]

      expect(search.findIn(nodes, 'hello')).to.be.eql([{
        id: 'hello world',
        match: '<em>hello</em> world',
        functions: [
          {id: 'hello world', match: '<em>hello</em> world', anchor: 'hello-world'}
        ]
      }, {
        id: 'world',
        match: 'world',
        functions: [
          {id: 'hello world', match: '<em>hello</em> world', anchor: 'hello-world'}
        ]
      }])
    })

    it('searches for callback matches', () => {
      var nodes = [
        {id: 'hello', callbacks: [{id: 'run'}]},
        {id: 'world'}
      ]

      expect(search.findIn(nodes, 'run')).to.be.eql([{
        id: 'hello',
        match: 'hello',
        callbacks: [
          {id: 'run', match: '<em>run</em>'}
        ]
      }])
    })

    it('searches for macro matches', () => {
      var nodes = [
        {id: 'hello', macros: [{id: 'run'}]},
        {id: 'world'}
      ]

      expect(search.findIn(nodes, 'run')).to.be.eql([{
        id: 'hello',
        match: 'hello',
        macros: [
          {id: 'run', match: '<em>run</em>'}
        ]
      }])
    })

    it('searches for nested matches', () => {
      var nodes = [
        {id: 'hello', macros: [{id: 'run'}]},
        {id: 'world'}
      ]

      expect(search.findIn(nodes, 'hello.run')).to.be.eql([{
        id: 'hello',
        match: 'hello',
        macros: [
          {id: 'run', match: 'run'}
        ]
      }])
    })
  })
})
