const { resolve } = require('path')

module.exports = {
  entry: {
    epub: [
      './assets/js/epub.js',
      './assets/less/epub.less'
    ],
    html: [
      './assets/js/app.js',
      './assets/less/app.less'
    ]
  },
  output: {
    path: resolve(__dirname, '../../formatters'),
    filename: '[name]/dist/[name].js'
  },
  resolve: {
    extensions: ['.js', '.less']
  },
  module: {
    rules: [
      {
        test: /\.handlebars$/,
        loader: 'handlebars-loader',
        query: {
          helperDirs: [
            resolve(__dirname, 'js', 'template-helpers')
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
      },
      {
        test: /\.less$/,
        use: ['style-loader', 'css-loader', 'postcss-loader', 'less-loader']
      },
      {
        test: /\.(eot|svg|ttf|woff)$/,
        use: [
          {
            loader: 'file-loader',
            options: {
              name: '[name].[ext]',
              outputPath: 'html/fonts/'
            }
          }
        ]
      }
    ]
  }
}
