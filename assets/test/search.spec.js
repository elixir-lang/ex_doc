import * as search from '../js/search'

describe('search', () => {
  describe('findIn', () => {
    it('returns a list of matches', () => {
      var nodes = [{id: 'hello world', title: 'hello world'},
                   {id: 'world', title: 'world'}]

      expect(search.findIn(nodes, 'hello')).to.be.eql([
        {id: 'hello world', match: '<em>hello</em> world'}
      ])
    })

    it('searches for function matches', () => {
      var nodes = [{
        id: 'id1',
        title: 'hello world',
        functions: [
          {id: 'hello world', anchor: 'hello-world'}
        ]
      }, {
        id: 'id2',
        title: 'world',
        functions: [
          {id: 'hello world', anchor: 'hello-world'}
        ]
      }, {
        id: 'world2',
        title: 'world2',
        functions: [
          {id: 'world', anchor: 'world'}
        ]
      }]

      expect(search.findIn(nodes, 'hello')).to.be.eql([{
        id: 'id1',
        match: '<em>hello</em> world',
        functions: [
          {id: 'hello world', match: '<em>hello</em> world', anchor: 'hello-world'}
        ]
      }, {
        id: 'id2',
        match: 'world',
        functions: [
          {id: 'hello world', match: '<em>hello</em> world', anchor: 'hello-world'}
        ]
      }])
    })

    it('searches for callback matches', () => {
      var nodes = [
        {id: 'hello', title: 'hello', callbacks: [{id: 'run'}]},
        {id: 'world', title: 'world'}
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
        {id: 'hello', title: 'hello', macros: [{id: 'run'}]},
        {id: 'world', title: 'world'}
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
        {id: 'hello', title: 'hello', macros: [{id: 'run'}]},
        {id: 'world', title: 'world'}
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
