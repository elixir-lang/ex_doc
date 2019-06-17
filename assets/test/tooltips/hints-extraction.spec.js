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

    it('extracts plain text, without html tags', () => {

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
          <h1 class="signature">
            configure(any)

            <a href="https://github.com/" class="view-source" rel="help" title="View Source">
              <span class="icon-code" aria-hidden="true"></span>
              <span class="sr-only">View Source</span>
            </a>

            <div class="specs">
              <pre>
                configure(<a href="https://hexdocs.pm/elixir/typespecs.html#basic-types">any</a>()) :: :ok
              </pre>
            </div>
          </h1>
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

  describe('extractTypeHint', () => {
    const typesPageContent = () => $($.parseHTML(`
      <div>
        <h3 id="basic-types" class="section-heading">
          <a href="#basic-types" class="hover-link"><span class="icon-link" aria-hidden="true"></span></a>
          Basic types
        </h3>

        <pre>
          <code>Basic types list</code>
        </pre>

        <h3 id="literals" class="section-heading">
          <a href="#literals" class="hover-link"><span class="icon-link" aria-hidden="true"></span></a>
          Literals
        </h3>

        <pre>
          <code>Literals list</code>
        </pre>

        <h3 id="built-in-types" class="section-heading">
          <a href="#built-in-types" class="hover-link"><span class="icon-link" aria-hidden="true"></span></a>
          Built-in types
        </h3>

        <table>
          <tr><td> byte()         </td><td> 0..255 </td></tr>
          <tr><td> my_type_name() </td><td> any()  </td></tr>
          <tr><td> function()     </td><td> fun()  </td></tr>
        </table>
      </div>
    `))

    it('extracts detailed info for built-in types', () => {
      let category = { name: 'builtInType', description: 'Built-in type', hash: '#built-in-types', detailsAvailable: true }
      let hint = extractTypeHint(typesPageContent(), 'my_type_name', category)

      expect(hint.kind).to.eql('type')
      expect(hint.title).to.eql('my_type_name()')
      expect(hint.description).to.eql('any()')
    })

    it('returns simple description for literals', () => {
      let category = { name: 'literal', description: 'Literal', hash: '#literals', detailsAvailable: false }
      let hint = extractTypeHint(typesPageContent(), '<<>>', category)

      expect(hint.kind).to.eql('type')
      expect(hint.title).to.eql('')
      expect(hint.description).to.eql('Literal')
    })

    it('returns simple description for built-in types', () => {
      let category = { name: 'basicType', description: 'Basic type', hash: '#basic-types', detailsAvailable: false }
      let hint = extractTypeHint(typesPageContent(), 'integer', category)

      expect(hint.kind).to.eql('type')
      expect(hint.title).to.eql('')
      expect(hint.description).to.eql('Basic type')
    })
  })
})
