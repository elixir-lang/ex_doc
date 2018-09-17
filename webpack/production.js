const UglifyJSPlugin = require('uglifyjs-webpack-plugin')
const merge = require('webpack-merge')
const common = require('./common.js')

module.exports = merge(common, {
  mode: 'production',
  output: {
    filename: '[name]/dist/[name]-[chunkhash].js'
  },
  optimization: {
    minimizer: [
      new UglifyJSPlugin({
        cache: true,
        parallel: true
      })
    ]
  }
})
