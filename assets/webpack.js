const TerserPlugin = require('terser-webpack-plugin')
const MiniCSSExtractPlugin = require('mini-css-extract-plugin')
const OptimizeCSSAssetsPlugin = require('optimize-css-assets-webpack-plugin')
const { CleanWebpackPlugin } = require('clean-webpack-plugin')
const { resolve } = require('path')

const pathsToClean = [
  './formatters/epub/dist',
  './formatters/html/dist',
  './formatters/html/fonts'
]

const cleanOptions = {
  root: resolve(__dirname, '..')
}

function buildFilename(name, extension) {
  name = name.split("-")
  folder = name[0]
  file = name[1]
  return folder + '/dist/' + file + '-[contenthash]' + extension
}

module.exports = {
  mode: 'production',
  entry: {
    "epub-app": './js/entry/epub.js',
    "epub-elixir": './less/entry/epub-elixir.less',
    "epub-erlang": './less/entry/epub-erlang.less',
    "html-app": './js/entry/html.js',
    "html-elixir": './less/entry/html-elixir.less',
    "html-erlang": './less/entry/html-erlang.less',
  },
  output: {
    path: resolve(__dirname, '../formatters'),
    filename: (chunkData) => buildFilename(chunkData.chunk.name, '.js')
  },
  resolve: {
    extensions: ['.js', '.less']
  },
  optimization: {
    minimize: true,
    minimizer: [
      new TerserPlugin(),
      new OptimizeCSSAssetsPlugin({})
    ]
  },
  module: {
    rules: [
      {
        test: /\.handlebars$/,
        loader: 'handlebars-loader',
        query: {
          helperDirs: [
            resolve(__dirname, 'js/handlebars/template-helpers')
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
        use: [MiniCSSExtractPlugin.loader, 'css-loader', 'postcss-loader', 'less-loader']
      },
      {
        test: /\.(eot|svg|ttf|woff|woff2)$/,
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
    new MiniCSSExtractPlugin({moduleFilename: ({name}) => buildFilename(name, '.css')}),
    new CleanWebpackPlugin({
      cleanOnceBeforeBuildPatterns: [
        './epub/dist',
        './html/dist',
        './html/fonts'
      ],
      cleanAfterEveryBuildPatterns: [
        './*/dist/erlang-*.js',
        './*/dist/elixir-*.js'
      ]
    })
  ]
}
