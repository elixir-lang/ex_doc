const { resolve } = require('path')

module.exports = {
  entry: {
    epub: ['./assets/js/epub.js'],
    html: ['./assets/js/app.js']
  },
  output: {
    path: resolve(__dirname, '../formatters'),
    filename: '[name]/dist/[name]-[hash].js'
  },
  resolve: {
    extensions: ['.js']
  },
  module: {
    rules: [
      {
        test: /\.handlebars$/,
        loader: 'handlebars-loader',
        query: {
          helperDirs: [
            __dirname + '/js/template-helpers'
          ]
        }
      }
    ]
  }
}
