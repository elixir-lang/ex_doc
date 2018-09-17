const CleanWebpackPlugin = require('clean-webpack-plugin')
const { resolve } = require('path')

const pathsToClean = [
  './formatters/epub/dist',
  './formatters/html/dist'
]

const cleanOptions = {
  root: resolve(__dirname, '..')
}

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
      },
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: ['@babel/preset-env'],
            cacheDirectory: true
          }
        }
      }
    ]
  },
  plugins: [
    new CleanWebpackPlugin(pathsToClean, cleanOptions)
  ]
}
