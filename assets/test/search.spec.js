var search = require('../js/search')

describe('search', function () {
  describe('findIn', function () {
    it('returns a list of matches', function () {
      var nodes = [{id: 'hello world'}, {id: 'world'}]

      expect(search.findIn(nodes, 'hello')).to.be.eql([
        {id: 'hello world', match: '<em>hello</em> world'}
      ])
    })

    it('searches for function matches', function () {
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
        id: 'world2', functions: [
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

    it('searches for callback matches', function () {
      var nodes = [
        {id: 'hello', callbacks: [{id: 'run'}]},
        {id: 'world'}
      ]

      expect(search.findIn(nodes, 'run')).to.be.eql([{
        id: 'hello', match: 'hello', callbacks: [
          {id: 'run', match: '<em>run</em>'}
        ]
      }])
    })

    it('searches for macro matches', function () {
      var nodes = [
        {id: 'hello', macros: [{id: 'run'}]},
        {id: 'world'}
      ]

      expect(search.findIn(nodes, 'run')).to.be.eql([{
        id: 'hello', match: 'hello', macros: [
          {id: 'run', match: '<em>run</em>'}
        ]
      }])
    })

    it('searches for nested matches', function () {
      var nodes = [
        {id: 'hello', macros: [{id: 'run'}]},
        {id: 'world'}
      ]

      expect(search.findIn(nodes, 'hello.run')).to.be.eql([{
        id: 'hello', match: 'hello', macros: [
          {id: 'run', match: 'run'}
        ]
      }])
    })
  })
})
