const UglifyJSPlugin = require('uglifyjs-webpack-plugin')
const MiniCSSExtractPlugin = require('mini-css-extract-plugin')
const OptimizeCSSAssetsPlugin = require('optimize-css-assets-webpack-plugin')
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
      }),
      new OptimizeCSSAssetsPlugin({})
    ]
  },
  module: {
    rules: [
      {
        test: /\.less$/,
        use: [MiniCSSExtractPlugin.loader, 'css-loader', 'postcss-loader', 'less-loader']
      }
    ]
  },
  plugins: [
    new MiniCSSExtractPlugin({
      filename: './[name]/dist/[name]-[chunkhash].css'
    })
  ]
})
