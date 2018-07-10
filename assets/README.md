# Assets

In this directory live all assets for `ExDoc`. The ready to use built
versions are found in `formatters/{html,epub}/dist`.

To work on these assets you need to install [Node.js] (version 10) and
[npm] (version 5.6) first (maybe as superuser or administrator).

## npm run scripts

The following scripts are available

### `build`

This will build a complete production bundle, including JavaScript and CSS.

### `clean`

Clean all content in the build folder `dist` for each format.

### `lint`

Lint all JavaScript files in `js` using [ESLint].

### `test`

Run all the available tests for JavaScript using [Karma].

### `watch`

Builds JavaScript and CSS on file changes in `assets/less/**/*.less` or
`assets/js/**/*.js` for each format.

## Gulp

Internally we use [gulp]. The project is organized in tasks
(`watch`, `build`, etc) and subtasks based on HTML and EPUB
formats. We also use [webpack] and [less].

[Node.js]: https://nodejs.org/
[npm]: https://www.npmjs.com/
[gulp]: https://www.npmjs.com/package/gulp
[webpack]: http://webpack.github.io/
[less]: http://lesscss.org/
[ESLint]: http://eslint.org/
[Karma]: http://karma-runner.github.io/
