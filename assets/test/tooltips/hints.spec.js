import { extractModuleHint, extractFunctionHint, isValidHintHref, getHint, HINT_KIND } from '../../js/tooltips/hints'

function setLocation (url) {
  delete window.location
  window.location = new URL(url)
}

describe('isValidHintHref', () => {
  beforeAll(() => setLocation('http://localhost/doc/gen_server.html'))

  it('accepts .html links with arity hash', () => {
    expect(isValidHintHref('http://localhost/doc/erlang.html#t:timeout/0')).toBe(true)
    expect(isValidHintHref('http://localhost/doc/gen_server.html#start_link/3')).toBe(true)
  })

  it('accepts extensionless links with arity hash', () => {
    expect(isValidHintHref('http://localhost/doc/erlang#t:timeout/0')).toBe(true)
    expect(isValidHintHref('http://localhost/doc/gen_server#start_link/3')).toBe(true)
  })

  it('accepts module links without hash', () => {
    expect(isValidHintHref('http://localhost/doc/gen_server.html')).toBe(true)
    expect(isValidHintHref('http://localhost/doc/gen_server')).toBe(true)
  })

  it('rejects section hashes without arity', () => {
    expect(isValidHintHref('http://localhost/doc/gen_server.html#functions')).toBe(false)
    expect(isValidHintHref('http://localhost/doc/gen_server#functions')).toBe(false)
  })

  it('rejects cross-origin links', () => {
    expect(isValidHintHref('http://other-host.com/erlang.html#t:timeout/0')).toBe(false)
  })

  it('accepts predefined hint hrefs', () => {
    expect(isValidHintHref('http://localhost/doc/typespecs.html#built-in-types')).toBe(true)
    expect(isValidHintHref('http://localhost/doc/typespecs#built-in-types')).toBe(true)
  })
})

describe('getHint predefined hints', () => {
  it('matches .html predefined hints', async () => {
    const hint = await getHint('http://localhost/doc/typespecs.html#basic-types')
    expect(hint).toEqual({ kind: HINT_KIND.plain, description: 'Basic type' })
  })

  it('matches extensionless predefined hints', async () => {
    const hint = await getHint('http://localhost/doc/typespecs#built-in-types')
    expect(hint).toEqual({ kind: HINT_KIND.plain, description: 'Built-in type' })
  })
})

describe('hints extraction', () => {
  describe('extractModuleHint', () => {
    const modulePageObject = parseHTML(`
      <div>
        <h1>
          <a href="https://github.com/" title="View Source" class="icon-action" rel="help">
            <i class="ri-code-s-slash-line" aria-hidden="true"></i>
            <span class="sr-only">View Source</span>
          </a>

          <span>Some module</span> <small class="app-vsn">(ExDoc v0.0.1)</small>
        </h1>
        <section id="moduledoc">
          <p>
            Module <strong>description</strong> here
          </p>
        </section>
        <section id="summary">List of functions with summaries</section>
      </div>
    `)[0]

    it('extracts hint info', () => {
      expect(extractModuleHint(modulePageObject).title).toBe('Some module')
      expect(extractModuleHint(modulePageObject).description).toBe('Module <strong>description</strong> here')
      expect(extractModuleHint(modulePageObject).kind).toBe('module')
    })
  })

  describe('extractFunctionHint', () => {
    const functionDetailObject = parseHTML(`
      <div>
        <div class="detail-header">
          <a href="#c:configure/1" class="detail-link" title="Link to this callback">
            <i class="ri-link-m" aria-hidden="true"></i>
            <span class="sr-only">Link to this callback</span>
          </a>

          <h1 class="signature">configure(any)</h1>

          <a href="https://github.com/" class="icon-action" rel="help" title="View Source">
            <i class="ri-code-s-slash-line" aria-hidden="true"></i>
            <span class="sr-only">View Source</span>
          </a>
        </div>
        <section class="docstring">
          <p>First line of description.</p>
          <p>Second line of description.</p>
        </section>
      </div>
    `)[0]

    it('extracts hint info', () => {
      const hint = extractFunctionHint(functionDetailObject)

      expect(hint.title).toBe('configure(any)')
      expect(hint.description).toBe('First line of description.')
      expect(hint.kind).toBe('function')
    })
  })
})

function parseHTML (html) {
  const doc = document.implementation.createHTMLDocument();
  doc.body.innerHTML = html;
  return doc.body.children;
}
