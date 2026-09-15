import lunr from 'lunr'
import { generateSnippet } from '../js/search-highlighting'

describe('full-text search highlighting', () => {
  const doc = 'A template renders templates.'
  const index = lunr(function () {
    this.ref('ref')
    this.field('doc')
    this.metadataWhitelist = ['position']
    this.add({ ref: 'guide', doc })
  })

  it('uses the original indexed text when highlighting stemmed matches', () => {
    const [result] = index.search('template')

    expect(Object.keys(result.matchData.metadata)).toEqual(['templat'])
    expect(index.search('templat')).toHaveLength(1)
    expect(generateSnippet(doc, result.matchData.metadata)).toBe('A <em>template</em> renders <em>templates</em>.')
  })

  it('highlights the whole original term for wildcard matches', () => {
    const [result] = index.search('templa*')

    expect(generateSnippet(doc, result.matchData.metadata)).toBe('A <em>template</em> renders <em>templates</em>.')
  })
})
