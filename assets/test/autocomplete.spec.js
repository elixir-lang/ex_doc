import * as autocomplete from '../js/autocomplete'

window.sidebarNodes = {
  exceptions: [{
    id: 'My exception',
    title: 'My exception'
  }],
  modules: [
    {
      id: 'Map',
      title: 'Map',
      nodeGroups: [
        {key: 'functions', nodes: [{id: 'fetch/2', anchor: 'fetch/2'}]}
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

describe('autocomplete', () => {
  describe('find', () => {
    it('finds matching modules, tasks and exceptions', () => {
      expect(autocomplete.find('Map').length).to.eql(1)
      expect(autocomplete.find('Ecto.Repo').length).to.eql(1)
      expect(autocomplete.find('phx.server').length).to.eql(1)
    })

    it('finds matching functions, callbacks and types', () => {
      expect(autocomplete.find('get_by').length).to.eql(1)
      expect(autocomplete.find('fetch').length).to.eql(1)
      expect(autocomplete.find('has_many').length).to.eql(1)
    })

    // TODO: Sorting
    // TODO: Sorting

    it('is case insensitive', () => {
      expect(autocomplete.find('My ExCePtIoN')).to.eql(autocomplete.find('my exception'))
    })

    it('highlights matched fragment', () => {
      const firstItemFound = autocomplete.find('get_')[0]

      expect(firstItemFound.title).to.eql('<em>get_</em>by/3')
    })

    it('returns max 5 results', () => {
      expect(autocomplete.find('e').length).to.eql(5)
    })

    it('returns no results if no match found', () => {
      expect(autocomplete.find('does not exist')).to.eql([])
    })

    it('returns no results if no search term provided', () => {
      expect(autocomplete.find()).to.eql([])
    })

    it('returns no results if search term consists of whitespace characters', () => {
      const searchTerm = '  '
      expect(autocomplete.find(searchTerm)).to.eql([])
    })
  })
})
