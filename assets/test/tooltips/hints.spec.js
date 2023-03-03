import { extractModuleHint, extractFunctionHint } from '../../js/tooltips/hints'

describe('hints extraction', () => {
  describe('extractModuleHint', () => {
    const modulePageObject = parseHTML(`
      <div>
        <h1>
          <button class="icon-action display-settings">
            <i class="ri-settings-3-line"></i>
            <span class="sr-only">Settings</span>
          </button>

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
      expect(extractModuleHint(modulePageObject).title).to.eql('Some module')
      expect(extractModuleHint(modulePageObject).description).to.eql('Module description here')
      expect(extractModuleHint(modulePageObject).kind).to.eql('module')
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

      expect(hint.title).to.eql('configure(any)')
      expect(hint.description).to.eql('First line of description.')
      expect(hint.kind).to.eql('function')
    })
  })
})

function parseHTML (html) {
  const doc = document.implementation.createHTMLDocument();
  doc.body.innerHTML = html;
  return doc.body.children;
}
