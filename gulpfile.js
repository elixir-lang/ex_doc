// Gulp File for ExDoc
// ===================

// Dependencies
// ------------

var gulp = require('gulp')
var $ = require('gulp-load-plugins')({camelize: true})
var Server = require('karma').Server

// Tasks
// -----

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

gulp.task('default', ['lint', 'test'])
