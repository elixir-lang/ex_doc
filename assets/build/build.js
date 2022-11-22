const path = require('node:path')
const process = require('node:process')
const child_process = require('node:child_process')
const esbuild = require('esbuild')
const util = require('./utilities')

const watchMode = Boolean(process.env.npm_config_watch)


/**
 * Configuration variables
 */

// Basic build configuration and values
const commonOptions = {
  entryNames: '[name]-[hash]',
  bundle: true,
  minify: true,
  logLevel: watchMode ? 'warning' : 'info',
}
const epubOutDir = path.resolve('../formatters/epub/dist')
const htmlOutDir = path.resolve('../formatters/html/dist')

// Handlebars template paths
const templates = {
  sourceDir: path.resolve('js/handlebars/templates'),
  compiledDir: path.resolve('../tmp/handlebars'),
  filename: 'handlebars.templates.js',
}
templates.compiledPath = path.join(templates.compiledDir, templates.filename)


/**
 * Build: Plugins
 */

// Empty outdir directories before both normal and watch-mode builds
const epubOnStartPlugin = {
  name: 'epubOnStart',
  setup(build) { build.onStart(() => util.ensureEmptyDirsExistSync([epubOutDir])) },
}
const htmlOnStartPlugin = {
  name: 'htmlOnStart',
  setup(build) { build.onStart(() => util.ensureEmptyDirsExistSync([htmlOutDir])) },
}


/**
 * Build
 */

// ePub: esbuild options
const epubBuildOptions = {
  ...commonOptions,
  outdir: epubOutDir,
  plugins: [epubOnStartPlugin],
  entryPoints: [
    'js/entry/epub.js',
    'css/entry/epub-elixir.css',
    'css/entry/epub-erlang.css',
  ],
}

// ePub: esbuild (conditionally configuring watch mode and rebuilding of docs)
if (!watchMode) {
  esbuild.build(epubBuildOptions).catch(() => process.exit(1))
} else {
  esbuild.build({
    ...epubBuildOptions,
    watch: {
      onRebuild(error, result) {
        if (error) {
          console.error('[watch] epub build failed:', error)
        } else {
          console.log('[watch] epub assets rebuilt')
          if (result.errors.length > 0) console.log('[watch] epub build errors:', result.errors)
          if (result.warnings.length > 0) console.log('[watch] epub build warnings:', result.warnings)
          generateDocs("epub")
        }
      },
    },
  }).then(() => generateDocs("epub")).catch(() => process.exit(1))
}

// HTML: Precompile Handlebars templates
util.runShellCmdSync(`npx handlebars ${templates.sourceDir} --output ${templates.compiledPath}`)

// HTML: esbuild options
const htmlBuildOptions = {
  ...commonOptions,
  outdir: htmlOutDir,
  plugins: [htmlOnStartPlugin],
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
}

// HTML: esbuild (conditionally configuring watch mode and rebuilding of docs)
if (!watchMode) {
  esbuild.build(htmlBuildOptions).catch(() => process.exit(1))
} else {
  esbuild.build({
    ...htmlBuildOptions,
    watch: {
      onRebuild(error, result) {
        if (error) {
          console.error('[watch] html build failed:', error)
        } else {
          console.log('[watch] html assets rebuilt')
          if (result.errors.length > 0) console.log('[watch] html build errors:', result.errors)
          if (result.warnings.length > 0) console.log('[watch] html build warnings:', result.warnings)
          buildTemplatesRuntime()
          generateDocs("html")
        }
      },
    },
  }).then(() => {
    buildTemplatesRuntime()
    generateDocs("html")
  }).catch(() => process.exit(1))
}

/**
 * Functions
 */

// HTML: Handlebars runtime
// The Handlebars runtime from the local module dist directory is used to ensure
// the version matches that which was used to compile the templates.
// 'bundle' must be false in order for 'Handlebar' to be available at runtime.
function buildTemplatesRuntime() {
  esbuild.build({
    ...commonOptions,
    outdir: htmlOutDir,
    entryPoints: ['node_modules/handlebars/dist/handlebars.runtime.js'],
    bundle: false,
  }).catch(() => process.exit(1))
}
// Build of the templates runtime is not required here when in watch mode, as
// the ebuild watch mode config already provides for such.
if (!watchMode) {
  buildTemplatesRuntime()
}

// Docs generation (used in watch mode only)
function generateDocs(formatter) {
  console.log(`Building ${formatter} docs`)
  process.chdir('../')
  child_process.execSync('mix compile --force')
  child_process.execSync(`mix docs --formatter ${formatter}`)
  process.chdir('./assets/')
}
