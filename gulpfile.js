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

var distPath = {
  html: 'priv/ex_doc/formatter/html/assets/dist',
  epub: 'priv/ex_doc/formatter/epub/assets/dist'
}

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

gulp.task('clean:html', function () {
  return del(distPath.html)
})

gulp.task('clean:epub', function () {
  return del(distPath.epub)
})

gulp.task('clean', function (done) {
  sequence(
    ['clean:html', 'clean:epub'],
    done
  )
})

gulp.task('javascript:html', ['buildHighlight'], function () {
  return javascript({src: 'assets/js/app.js', dest: distPath.html})
})

gulp.task('javascript:epub', ['buildHighlight'], function () {
  return javascript({src: 'assets/js/epub.js', dest: distPath.epub})
})

gulp.task('javascript', function (done) {
  sequence(
    ['javascript:html', 'javascript:epub'],
    done
  )
})

gulp.task('less:html', function () {
  return less({src: 'assets/less/app.less', dest: distPath.html})
})

gulp.task('less:epub', function () {
  return less({src: 'assets/less/epub.less', dest: distPath.epub})
})

gulp.task('less', function (done) {
  sequence(
    ['less:html', 'less:epub'],
    done
  )
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

gulp.task('build:html', function (done) {
  sequence(
    'clean:html',
    ['javascript:html', 'less:html'],
    done
  )
})

gulp.task('build:epub', function (done) {
  sequence(
    'clean:epub',
    ['javascript:epub', 'less:epub'],
    done
  )
})

gulp.task('build', function (done) {
  sequence(
    ['build:html', 'build:epub'],
    done
  )
})

gulp.task('watch', function (done) {
  var source = {
    html: {
      js: ['./assets/js/**/*.js', '!./assets/js/epub.js'],
      less: ['./assets/less/**/*.less', '!./assets/less/epub.less']
    },
    epub: {
      js: ['./assets/js/**/*.js', '!./assets/js/app.js'],
      less: ['./assets/less/**/*.less', '!./assets/less/app.less']
    }
  }

  // Watch JS for HTML formatter
  $.watch(source.html.js, $.batch(function (events, done) {
    events
      .on('data', gulp.start('javascript:html', done))
      .on('end', done)
  }))

  // Watch LESS for HTML formatter
  $.watch(source.html.less, $.batch(function (events, done) {
    events
      .on('data', gulp.start('less:html', done))
      .on('end', done)
  }))

  // Watch JS for EPUB formatter
  $.watch(source.epub.js, $.batch(function (events, done) {
    events
      .on('data', gulp.start('javascript:epub', done))
      .on('end', done)
  }))

  // Watch LESS for HTML formatter
  $.watch(source.epub.less, $.batch(function (events, done) {
    events
      .on('data', gulp.start('less:epub', done))
      .on('end', done)
  }))
})

gulp.task('default', ['lint', 'test'])

/**
 * Helpers
 */
var javascript = function (options) {
  return gulp.src(options.src)
    .pipe(webpack(isProduction ? config.production : config.development))
    .pipe($.if(isProduction, $.uglify()))
    .pipe($.if(isProduction, $.rev()))
    .pipe($.size({title: 'js'}))
    .pipe(gulp.dest(options.dest))
}

var less = function (options) {
  return gulp.src(options.src)
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
    .pipe(gulp.dest(options.dest))
}
