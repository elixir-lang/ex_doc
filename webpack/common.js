const CleanWebpackPlugin = require('clean-webpack-plugin')
const { resolve } = require('path')

const pathsToClean = [
  './formatters/epub/dist',
  './formatters/html/dist',
  './formatters/html/fonts'
]

const cleanOptions = {
  root: resolve(__dirname, '..')
}

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
    path: resolve(__dirname, '../formatters'),
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
  },
  plugins: [
    new CleanWebpackPlugin(pathsToClean, cleanOptions)
  ]
}
