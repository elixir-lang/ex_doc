const path = require('node:path')
const esbuild = require('esbuild')
const util = require('./utilities')


/**
 * Configuration variables
 */

// Build options
const commonOptions = {
  entryNames: '[name]-[hash]',
  bundle: true,
  minify: true,
  logLevel: 'info',
}
const epubOptions = {
  outdir: path.resolve('../formatters/epub/dist'),
}
const htmlOptions = {
  outdir: path.resolve('../formatters/html/dist'),
}

// Handlebars template paths
const templates = {
  sourceDir: path.resolve('js/handlebars/templates'),
  compiledDir: path.resolve('../tmp/handlebars'),
  filename: 'handlebars.templates.js',
}
templates.compiledPath = path.join(templates.compiledDir, templates.filename)

// Directories to create/clean
const cleanDirs = {
  pre: [
    epubOptions.outdir,
    htmlOptions.outdir,
    templates.compiledDir,
  ],
  postHtml: [
    templates.compiledDir,
  ],
}


/**
 * Clean: pre-build
 */

util.ensureEmptyDirsExistSync(cleanDirs.pre)


/**
 * Build: ePub
 */

const epubBuild = esbuild.build({
  ...commonOptions,
  ...epubOptions,
  entryPoints: [
    'js/entry/epub.js',
    'css/entry/epub-elixir.css',
    'css/entry/epub-erlang.css',
  ],
}).catch(() => process.exit(1))


/**
 * Build: HTML
 */

// Precompile Handlebars templates
util.runShellCmdSync(`npx handlebars ${templates.sourceDir} --output ${templates.compiledPath}`)

// esbuild
const htmlBuild = esbuild.build({
  ...commonOptions,
  ...htmlOptions,
  entryPoints: [
    templates.compiledPath,
    'js/entry/html.js',
    'css/entry/html-elixir.css',
    'css/entry/html-erlang.css',
  ],
  loader: {
    '.woff2': 'file',
    // TODO: Remove when @fontsource/* removes legacy .woff
    '.woff': 'file',
  },
}).catch(() => process.exit(1))

// The Handlebars runtime from the local module dist directory is used to ensure
// the version matches that which was used to compile the templates.
// 'bundle' must be false in order for 'Handlebar' to be available at runtime.
esbuild.build({
  ...commonOptions,
  ...htmlOptions,
  entryPoints: ['node_modules/handlebars/dist/handlebars.runtime.js'],
  bundle: false,
}).catch(() => process.exit(1))


/**
 * Clean: post-build
 */
Promise.all([epubBuild, htmlBuild]).then(() => {
  util.ensureEmptyDirsExistSync(cleanDirs.postHtml)
})
