module.exports = {
  output: {
    filename: 'app.js'
  },
  module: {
    loaders: [
      // for handlebars
      { test: /\.handlebars$/, loader: 'handlebars-loader' }
    ]
  }
}
