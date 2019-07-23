const merge = require('webpack-merge')
const common = require('./common.js')

module.exports = merge(common, {
  mode: 'development',
  devtool: 'cheap-module-eval-source-map'
})
