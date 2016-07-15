# Assets

In this directory reside all assets for `ExDoc`. The ready to use built
versions are found in `priv/ex_doc/formatter/html/templates/dist`. To change any
of them read please read the following instructions:

## Dependencies

To work on these assets you need to install [Node.js] and [npm] first (probaby
as superuser or administrator). After that execute the following commands:

```bash
$ npm install -g gulp
$ npm install
```

Now many gulp tasks are available via the `gulp` command line.

## Available [gulp] tasks

If you run [gulp] without any option by default you will lint all JavaScript
files using [ESLint] and then you will run all the available tests for
JavaScript using [Karma].

### `build`

This will build a complete bundle, including JavaScript and CSS.

Using the flag `--type production` will result in minified JavaScript and CSS
bundles.

### `development`

Builds JavaScript and CSS on file changes. See `javascript-watch` and `less-watch`
for details.

### `javascript`

Build the JavaScript in `js` into a bundled file using [webpack].

### `javascript-watch`

Run `javascript` on file changes in `assets/js/*.js`.

### `less`

Build the [less] files in `less` into a bundled CSS file.

### `less-watch`

Run `less` on file changes in `assets/less/*.js`.

### `lint`

Lint all JavaScript files in `js` using [ESLint].

### `clean`

Clean all content in the build folder `dist`.

### `test`

Run all the available tests for JavaScript using [Karma].

[Node.js]: https://nodejs.org/
[npm]: https://www.npmjs.com/
[gulp]: https://www.npmjs.com/package/gulp
[webpack]: http://webpack.github.io/
[less]: http://lesscss.org/
[ESLint]: http://eslint.org/
[Karma]: http://karma-runner.github.io/
