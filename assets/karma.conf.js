module.exports = function (config) {
  config.set({
    basePath: '',

    frameworks: ['mocha', 'chai'],

    files: [
      'test/**/*.spec.js'
    ],

    preprocessors: {
      'test/**/*.spec.js': ['esbuild', 'sourcemap']
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
