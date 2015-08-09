# Assets

In this directory reside all assets for ExDoc. The ready to use built versions are found in `dist`. To change any of them read on

## Dependencies

To work on these assets you need to install [Node.js] and [npm]. After that run

```bash
$ npm install -g gulp
$ npm install
```

Now the following tasks are available

## Available [gulp] tasks

### `build`

This will build a complete bundle, including JavaScript and CSS.

Using the flag `--type production` will result in minified JavaScript and CSS
bundles.

Using the flag `--watch` a file watcher will be changed that recompiles the JavaScript
files on any change in the `js` folder.

### `javascript`

Build the JavaScript in `js` into a bundled file using [webpack].


### `less`

Build the [less] files in `less` into a bundled CSS file.

### `lint`

Lint all JavaScript files in `js` using [ESLint].

### `clean`

Clean all content in the build folder `dist`.



[Node.js]: https://nodejs.org/
[npm]: https://www.npmjs.com/
[gulp]: https://www.npmjs.com/package/gulp
[webpack]: http://webpack.github.io/
[less]: http://lesscss.org/
[ESLint]: http://eslint.org/
