import { getSuggestions } from '../../js/autocomplete/suggestions'

describe('getSuggestions', () => {
  beforeAll(() => {
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
          sections: [
            {id: 'The standard library', anchor: 'module-the-standard-library'},
            {id: 'Guards', anchor: 'module-guards'}
          ],
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
      extras: [
        {
          id: 'api-reference',
          title: 'API Reference'
        },
        {
          id: "how-to-make-things",
          title: "How to make things",
          searchData: [
            {
              id: "custom-text",
              anchor: "Some Custom Text",
              labels: ["custom"]
            }
          ]
        },
        {
          id: 'library-guidelines',
          title: 'Library Guidelines',
          headers: [
            {id: 'Getting started', anchor: 'getting-started'},
            {id: 'Publishing', anchor: 'publishing'}
          ]
        }
      ]
    }
  })

  describe('find', () => {
    it('returns matching modules, tasks, extras and exceptions', () => {
      expect(getSuggestions('Map')).toHaveLength(1)
      expect(getSuggestions('Ecto.Repo')).toHaveLength(1)
      expect(getSuggestions('phx.server')).toHaveLength(1)
      expect(getSuggestions('API Reference')).toHaveLength(1)
      expect(getSuggestions('My exception')).toHaveLength(1)
    })

    it('returns matching sections of modules and extras', () => {
      expect(getSuggestions('Getting')).toHaveLength(1)
      expect(getSuggestions('Gua')).toHaveLength(1)
    })

    it('returns custom searchData of extras', () => {
      expect(getSuggestions('custom text')).toHaveLength(1)
    })

    it('returns matching functions, callbacks and types', () => {
      expect(getSuggestions('get_by')).toHaveLength(1)
      expect(getSuggestions('fetch')).toHaveLength(1)
      expect(getSuggestions('has_many')).toHaveLength(1)
    })

    it('handles special characters', () => {
      expect(getSuggestions('&&/2')).toHaveLength(1)
      expect(getSuggestions('<>')).toHaveLength(1)
      expect(getSuggestions('@')).toHaveLength(1)
      expect(getSuggestions('match?')).toHaveLength(1)
    })

    it('is case insensitive', () => {
      expect(getSuggestions('My ExCePtIoN')).toHaveLength(1)
      expect(getSuggestions('My ExCePtIoN')).toEqual(getSuggestions('my exception'))
    })

    it('returns max 8 results', () => {
      expect(getSuggestions('e')).toHaveLength(10)
    })

    it('returns no results if no match found', () => {
      expect(getSuggestions('does not exist')).toEqual([])
    })

    it('returns no results if no search term provided', () => {
      expect(getSuggestions('')).toEqual([])
    })

    it('returns no results if search term consists of whitespace characters', () => {
      const searchTerm = '  '
      expect(getSuggestions(searchTerm)).toEqual([])
    })

    it('highlights matched fragment', () => {
      expect(getSuggestions('get')[0].title).toBe('<em>get</em>_by/3')
      expect(getSuggestions('Repo.get_')[0].title).toBe('<em>get_</em>by/3')
      expect(getSuggestions('Ecto.Repo.get_by')[0].title).toBe('<em>get_by</em>/3')
      expect(getSuggestions('po.get_by')[0].title).toBe('<em>get_by</em>/3')
    })

    it('sorts results, putting closer matches at the top', () => {
      const results = getSuggestions('insert')
      expect(results[0].title).toBe('<em>insert</em>/2')
      expect(results[1].title).toBe('<em>insert</em>_all/3')
    })

    it('marks callbacks with a special label', () => {
      const firstResult = getSuggestions('get_by')[0]

      expect(firstResult.labels).toEqual(['callback'])
    })

    it('marks types with a special label', () => {
      const firstResult = getSuggestions('has_many')[0]

      expect(firstResult.labels).toEqual(['type'])
    })

    it('includes module name in function description', () => {
      const firstResult = getSuggestions('get_by')[0]

      expect(firstResult.description).toBe('Ecto.Repo')
    })

    it('generates a link for each suggestion', () => {
      expect(getSuggestions('has_many')[0].link).toBe('Ecto.Schema.html#t:has_many/1')
      expect(getSuggestions('map')[0].link).toBe('Map.html')
      expect(getSuggestions('fetch')[0].link).toBe('Map.html#fetch/2')
    })
  })
})
