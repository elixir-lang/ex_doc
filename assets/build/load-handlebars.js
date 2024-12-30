const fs = require('node:fs/promises')
const handlebars = require('handlebars')

module.exports = {
  name: 'load-handlebars',
  /**
   * @param {import('esbuild').PluginBuild} build
   */
  setup (build) {
    build.onLoad({
      filter: /\.handlebars$/
    }, async ({ path: filename }) => {
      try {
        const source = await fs.readFile(filename, 'utf-8')
        const template = handlebars.precompile(source)
        const contents = [
          "import * as Handlebars from 'handlebars/runtime'",
          "import '../helpers'",
          `export default Handlebars.template(${template})`
        ].join('\n')
        return { contents }
      } catch (error) {
        return { errors: [{ text: error.message }] }
      }
    })
  }
}
