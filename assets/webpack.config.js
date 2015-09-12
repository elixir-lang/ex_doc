var webpack = require('webpack')

var output = {
  filename: 'app.js'
}

var loaders = [{
  test: /\.handlebars$/,
  loader: 'handlebars-loader',
  query: {
    helperDirs: [
      __dirname + '/js/template-helpers'
    ]
  }
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
