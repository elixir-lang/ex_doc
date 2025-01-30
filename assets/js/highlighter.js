import { escapeRegexModifiers, escapeHtmlEntities } from './helpers'

/**
 * Returns an HTML string highlighting the individual tokens from the query string.
 */
export function highlightMatches (text, query, opts = {}) {
  // Sort terms length, so that the longest are highlighted first.
  if (typeof query === 'string') {
    query = query.split(/\s+/)
  }
  const terms = query.sort((term1, term2) => term2.length - term1.length)
  return highlightTerms(text, terms, opts)
}

function highlightTerms (text, terms, opts) {
  if (terms.length === 0) return text

  let flags = 'i'

  if (opts.multiline) {
    flags = 'is'
  }

  const [firstTerm, ...otherTerms] = terms
  const match = text.match(new RegExp(`(.*)(${escapeRegexModifiers(firstTerm)})(.*)`, flags))

  if (match) {
    const [, before, matching, after] = match
    // Note: this has exponential complexity, but we expect just a few terms, so that's fine.
    return highlightTerms(before, terms, opts) + '<em>' + escapeHtmlEntities(matching) + '</em>' + highlightTerms(after, terms, opts)
  } else {
    return highlightTerms(text, otherTerms, opts)
  }
}
