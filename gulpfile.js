// Gulp File for ExDoc
// ===================

// Dependencies
// ------------

var gulp = require('gulp')
var $ = require('gulp-load-plugins')({camelize: true})
var sequence = require('run-sequence')
var del = require('del')
var LessPluginNpmImport = require('less-plugin-npm-import')
var LessPluginAutoPrefix = require('less-plugin-autoprefix')
var Server = require('karma').Server
var webpack = require('webpack-stream')
var exec = require('child_process').exec

var config = require('./assets/webpack.config')

// Config
// ------

// Set variable via $ gulp --type production
var environment = $.util.env.type || 'development'
var isProduction = environment === 'production'

var distPath = 'priv/ex_doc/formatter/html/templates/dist'

var npmPlugin = new LessPluginNpmImport()
var autoprefixPlugin = new LessPluginAutoPrefix({
  browsers: ['last 2 versions']
})

var languages = [
  'bash',
  'css',
  'diff',
  'elixir',
  'erlang',
  'erlang-repl',
  'xml',
  'http',
  'javascript',
  'json',
  'markdown',
  'sql'
]

// Tasks
// -----

gulp.task('buildHighlight', function (done) {
  exec('npm install', {
    cwd: './node_modules/highlight.js'
  }, function (err, stdout, stderr) {
    if (err) return done(err)

    exec('node tools/build.js -n ' + languages.join(' '), {
      cwd: './node_modules/highlight.js'
    }, function (err, stdout, stderr) {
      if (err) return done(err)

      done()
    })
  })
})

gulp.task('clean', function () {
  return del(distPath)
})

gulp.task('javascript', ['buildHighlight'], function () {
  return gulp.src('assets/js/app.js')
    .pipe(webpack(isProduction ? config.production : config.development))
    .pipe($.if(isProduction, $.uglify()))
    .pipe($.if(isProduction, $.rev()))
    .pipe($.size({title: 'js'}))
    .pipe(gulp.dest(distPath))
})

gulp.task('javascript-watch', ['buildHighlight'], function () {
  config.development.watch = true

  return gulp.src('assets/js/app.js')
    .pipe($.plumber())
    .pipe(webpack(config.development))
    .pipe($.size({title: 'js'}))
    .pipe(gulp.dest(distPath))
})

gulp.task('less', function () {
  return gulp.src('assets/less/app.less')
    .pipe($.less({
      plugins: [
        npmPlugin,
        autoprefixPlugin
      ]
    }))
    .pipe($.plumber())
    .pipe($.if(isProduction, $.minifyCss({
      compatibility: 'ie8',
      processImport: false
    })))
    .pipe($.if(isProduction, $.rev()))
    .pipe($.size({title: 'less'}))
    .pipe(gulp.dest(distPath))
})

gulp.task('less-watch', function () {
  $.watch('assets/less/**/*.less', $.batch(function (events, done) {
    gulp.start('less', done)
  }))
})

gulp.task('lint', function () {
  return gulp.src([
    'gulpfile.js',
    'assets/**/*.js'
  ])
    .pipe($.eslint())
    .pipe($.eslint.format())
    .pipe($.eslint.failOnError())
})

gulp.task('test', function (done) {
  new Server({
    configFile: __dirname + '/assets/karma.conf.js',
    singleRun: true
  }, done).start()
})

gulp.task('build', function (done) {
  sequence(
    'clean',
    ['javascript', 'less'],
    done
  )
})

gulp.task('development', function (done) {
  sequence(
    ['javascript-watch', 'less-watch'],
    done
  )
})

gulp.task('default', ['lint', 'test'])
