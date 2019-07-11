import {extractTypeHint, extractModuleHint, extractFunctionHint} from '../../js/tooltips/hints-extraction'
import $ from 'jquery'

describe('hints extraction', () => {
  describe('extractModuleHint', () => {
    var modulePageObject = $($.parseHTML(`
      <div>
        <h1>
          <small class="app-vsn">ex_doc v0.0.1</small>
          Some module
          <a href="https://github.com/" title="View Source" class="view-source" rel="help">
            <span class="icon-code" aria-hidden="true"></span>
            <span class="sr-only">View Source</span>
          </a>
        </h1>
        <section id="moduledoc">
          <p>
            Module <strong>description</strong> here
          </p>
        </section>
        <section id="summary">List of functions with summaries</section>
      </div>
    `))

    it('extracts hint info', () => {
      expect(extractModuleHint(modulePageObject).title).to.eql('Some module')
      expect(extractModuleHint(modulePageObject).description).to.eql('Module description here')
      expect(extractModuleHint(modulePageObject).kind).to.eql('module')
    })
  })

  describe('extractFunctionHint', () => {
    var functionDetailObject = $($.parseHTML(`
      <div>
        <div class="detail-header">
          <a href="#c:configure/1" class="detail-link" title="Link to this callback">
            <span class="icon-link" aria-hidden="true"></span>
            <span class="sr-only">Link to this callback</span>
          </a>

          <h1 class="signature">configure(any)</h1>

          <a href="https://github.com/" class="view-source" rel="help" title="View Source">
            <span class="icon-code" aria-hidden="true"></span>
            <span class="sr-only">View Source</span>
          </a>

          <div class="specs">
            <pre>
              configure(<a href="https://hexdocs.pm/elixir/typespecs.html#basic-types">any</a>()) :: :ok
            </pre>
          </div>
        </div>
        <section class="docstring">
          <p>First line of description.</p>
          <p>Second line of description.</p>
        </section>
      </div>
    `))

    it('extracts hint info', () => {
      let hint = extractFunctionHint(functionDetailObject)

      expect(hint.title).to.eql('configure(any)')
      expect(hint.description).to.eql('First line of description.')
      expect(hint.kind).to.eql('function')
      expect(hint.signatureSpecs).to.eql('configure(any()) :: :ok')
    })
  })
})
