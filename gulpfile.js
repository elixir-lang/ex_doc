// Gulp File for ExDoc
// ===================

// Dependencies
// ------------

var gulp = require('gulp')
var Server = require('karma').Server

// Tasks
// -----

gulp.task('test', function (done) {
  new Server({
    configFile: __dirname + '/assets/karma.conf.js',
    singleRun: true
  }, done).start()
})

gulp.task('default', ['test'])
