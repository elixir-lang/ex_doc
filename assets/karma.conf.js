var webpackConfig = require('./webpack.config')

module.exports = function (config) {
  config.set({
    basePath: '',

    frameworks: ['mocha', 'chai'],

    files: [
      'test/*.spec.js'
    ],

    preprocessors: {
      'test/*.spec.js': ['webpack', 'sourcemap']
    },

    webpack: {
      devtool: 'inline-source-map',
      debug: true,
      module: webpackConfig.module
    },

    webpackMiddleware: {
      noInfo: true
    },

    reporters: ['progress'],
    port: 9876,
    colors: true,
    logLevel: config.LOG_INFO,
    autoWatch: false,
    browsers: [process.env.TRAVIS ? 'Firefox' : 'Chrome'],
    singleRun: false
  })
}
