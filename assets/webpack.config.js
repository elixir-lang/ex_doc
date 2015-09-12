var webpack = require('webpack')

var output = {
  filename: 'app.js'
}

var loaders = [{
  test: /\.handlebars$/,
  loader: 'handlebars-loader'
}, {
  test: /\.js$/,
  exclude: /node_modules/,
  loader: 'babel',
  query: {
    optional: ['runtime'],
    stage: 0
  }
}]

module.exports = {
  development: {
    debug: true,
    devtool: 'eval-source-map',
    output: output,
    module: {
      loaders: loaders
    }
  },
  production: {
    output: output,
    module: {
      loaders: loaders
    },
    plugins: [
      new webpack.optimize.DedupePlugin()
    ]
  }
}
