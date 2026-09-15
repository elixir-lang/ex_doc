import { escapeHtmlEntities } from './helpers'

export function generateSnippet (doc, metadata) {
  const paragraphs = doc.split(/\r?\n\r?\n/)
  const skipHeading = paragraphs.length >= 2 && paragraphs[0].trim().startsWith('#')
  const paragraph = paragraphs[skipHeading ? 1 : 0]
  const paragraphStart = skipHeading ? doc.indexOf(paragraph, paragraphs[0].length) : 0
  const truncated = paragraph.slice(0, 200)
  const positions = matchPositions(doc, metadata, paragraphStart, truncated.length)

  let result = ''
  let cursor = 0

  for (const [start, length] of positions) {
    result += escapeHtmlEntities(truncated.slice(cursor, start))
    result += `<em>${escapeHtmlEntities(truncated.slice(start, start + length))}</em>`
    cursor = start + length
  }

  result += escapeHtmlEntities(truncated.slice(cursor))
  return paragraph.length > 200 ? result + '...' : result
}

function matchPositions (doc, metadata, snippetStart, snippetLength) {
  const positions = Object.values(metadata)
    .flatMap(fields => fields.doc?.position || [])
    .map(position => trimPosition(doc, position))
    .map(([start, length]) => [start - snippetStart, length])
    .filter(([start, length]) => start >= 0 && start + length <= snippetLength)
    .sort(([left], [right]) => left - right)

  return positions.reduce((merged, [start, length]) => {
    const previous = merged[merged.length - 1]

    if (!previous || start >= previous[0] + previous[1]) {
      merged.push([start, length])
    } else {
      previous[1] = Math.max(previous[0] + previous[1], start + length) - previous[0]
    }

    return merged
  }, [])
}

function trimPosition (doc, [start, length]) {
  const token = doc.slice(start, start + length)
  const leading = token.match(/^[^@:\w]+/)?.[0].length || 0
  const trailing = token.slice(leading).match(/[^\?\!\w]+$/)?.[0].length || 0

  return [start + leading, length - leading - trailing]
}
