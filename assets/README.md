# Assets

In this directory live all assets for `ExDoc`. The ready to use built
versions are found in `priv/ex_doc/formatter/{html,epub}/assets/dist`. To
change any of them read please read the following instructions:

## Dependencies

To work on these assets you need to install [Node.js] and [npm] first (probably
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

### Common tasks

The following tasks do not depend on the format (HTML or EPUB) that you're
working on.

#### `build`

This will build a complete bundle, including JavaScript and CSS.

Using the flag `--type production` will result in minified JavaScript and CSS
bundles.

#### `clean`

Clean all content in the build folder `dist` for each format.

#### `javascript`

Build the JavaScript in `js` into a bundled file using [webpack] for each
format.

#### `less`

Build the [less] files in `less` into a bundled CSS file for each format.

#### `lint`

Lint all JavaScript files in `js` using [ESLint].

#### `test`

Run all the available tests for JavaScript using [Karma].

#### `watch`

Builds JavaScript and CSS on file changes in `assets/less/**/*.less` or
`assets/js/**/*.js` for each format.

### Sub-Tasks used with the HTML format

If you're working with the HTML format, you should use the following tasks:

#### `build:html`

Same as the `build` task, but this one apply for the HTML format.

#### `clean:html`

Clean all content in the build folder `dist` of the HTML format.

#### `javascript:html`

Same as the `javascript` task, but this one apply for the HTML format.

#### `less:html`

Same as the `less` task, but this one apply for the HTML format.

### Sub-Tasks used with the EPUB format

If you're working with the EPUB format, you should use the following tasks:

#### `build:epub`

Same as the `build` task, but this one apply for the EPUB format.

#### `clean:epub`

Clean all content in the build folder `dist` of the EPUB format.

#### `javascript:epub`

Same as the `javascript` task, but this one apply for the EPUB format.

#### `less:epub`

Same as the `less:html` task, but this one apply for the EPUB format.

[Node.js]: https://nodejs.org/
[npm]: https://www.npmjs.com/
[gulp]: https://www.npmjs.com/package/gulp
[webpack]: http://webpack.github.io/
[less]: http://lesscss.org/
[ESLint]: http://eslint.org/
[Karma]: http://karma-runner.github.io/
