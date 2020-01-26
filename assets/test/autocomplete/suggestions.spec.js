import {getSuggestions} from '../../js/autocomplete/suggestions'

describe('getSuggestions', () => {
  before(() => {
    window.sidebarNodes = {
      modules: [
        {
          id: 'My exception',
          title: 'My exception',
          group: 'Exceptions'
        },
        {
          id: 'Map',
          title: 'Map',
          nodeGroups: [
            {key: 'functions', nodes: [{id: 'fetch/2', anchor: 'fetch/2'}]}
          ]
        },
        {
          id: 'Kernel',
          title: 'Kernel',
          nodeGroups: [
            {
              key: 'functions',
              nodes: [
                {id: '@/1', anchor: '@/1'},
                {id: '&&/2', anchor: '&&/2'},
                {id: '<>/2', anchor: '<>/2'},
                {id: 'match?/1', anchor: 'match?/1'}
              ]
            }
          ]
        },
        {
          id: 'Ecto.Repo',
          title: 'Ecto.Repo',
          nodeGroups: [
            {
              key: 'callbacks',
              nodes: [
                {id: 'get_by/3', anchor: 'c:get_by/3'},
                {id: 'insert/2', anchor: 'c:insert/2'},
                {id: 'insert_all/3', anchor: 'c:insert_all/3'}
              ]
            }
          ]
        },
        {
          id: 'Ecto.Schema',
          title: 'Ecto.Schema',
          nodeGroups: [
            {key: 'types', nodes: [{ id: 'has_many/1', anchor: 't:has_many/1' }]}
          ]
        }
      ],
      tasks: [{id: 'phx.server', title: 'phx.server'}],
      extras: [{id: 'api-reference', title: 'API Reference'}]
    }
  })

  describe('find', () => {
    it('returns matching modules, tasks and exceptions', () => {
      expect(getSuggestions('Map').length).to.eql(1)
      expect(getSuggestions('Ecto.Repo').length).to.eql(1)
      expect(getSuggestions('phx.server').length).to.eql(1)
      expect(getSuggestions('My exception').length).to.eql(1)
    })

    it('returns matching functions, callbacks and types', () => {
      expect(getSuggestions('get_by').length).to.eql(1)
      expect(getSuggestions('fetch').length).to.eql(1)
      expect(getSuggestions('has_many').length).to.eql(1)
    })

    it('ignores matching extras', () => {
      expect(getSuggestions('api-reference').length).to.eql(0)
    })

    it('handles special characters', () => {
      expect(getSuggestions('&&/2').length).to.eql(1)
      expect(getSuggestions('<>').length).to.eql(1)
      expect(getSuggestions('@').length).to.eql(1)
      expect(getSuggestions('match?').length).to.eql(1)
    })

    it('is case insensitive', () => {
      expect(getSuggestions('My ExCePtIoN').length).to.eql(1)
      expect(getSuggestions('My ExCePtIoN')).to.eql(getSuggestions('my exception'))
    })

    it('returns max 5 results', () => {
      expect(getSuggestions('e').length).to.eql(5)
    })

    it('returns no results if no match found', () => {
      expect(getSuggestions('does not exist')).to.eql([])
    })

    it('returns no results if no search term provided', () => {
      expect(getSuggestions()).to.eql([])
    })

    it('returns no results if search term consists of whitespace characters', () => {
      const searchTerm = '  '
      expect(getSuggestions(searchTerm)).to.eql([])
    })

    it('highlights matched fragment', () => {
      expect(getSuggestions('get')[0].title).to.eql('<em>get</em>_by/3')
      expect(getSuggestions('Repo.get_')[0].title).to.eql('<em>get_</em>by/3')
      expect(getSuggestions('Ecto.Repo.get_by')[0].title).to.eql('<em>get_by</em>/3')
      expect(getSuggestions('po.get_by')[0].title).to.eql('<em>get_by</em>/3')
    })

    it('sorts results, putting closer matches at the top', () => {
      let results = getSuggestions('insert')
      expect(results[0].title).to.eql('<em>insert</em>/2')
      expect(results[1].title).to.eql('<em>insert</em>_all/3')
    })

    it('marks callbacks with a special label', () => {
      const firstResult = getSuggestions('get_by')[0]

      expect(firstResult.label).to.eql('callback')
    })

    it('marks types with a special label', () => {
      const firstResult = getSuggestions('has_many')[0]

      expect(firstResult.label).to.eql('type')
    })

    it('includes module name in function description', () => {
      const firstResult = getSuggestions('get_by')[0]

      expect(firstResult.description).to.eql('Ecto.Repo')
    })

    it('generates a link for each suggestion', () => {
      expect(getSuggestions('has_many')[0].link).to.eql('Ecto.Schema.html#t:has_many/1')
      expect(getSuggestions('map')[0].link).to.eql('Map.html')
      expect(getSuggestions('fetch')[0].link).to.eql('Map.html#fetch/2')
    })
  })
})
