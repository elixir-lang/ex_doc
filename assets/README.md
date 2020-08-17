# Assets

In this directory live all assets for `ExDoc`. The built, ready-to-use
versions are found in `formatters/{html,epub}/dist`.

To work on these assets you need to install [Node.js] (version 10) and
[NPM] (version 5.6) first (maybe as superuser or administrator).

## `npm run` scripts

The following scripts are available from the root folder of the project.

### `build`

```bash
$ npm run --prefix assets build
```

This will build a complete production bundle, including JavaScript and CSS.
If you run `mix build` at the `ExDoc` root after changing your assets, it will
automatically recompile the assets, invoke `mix compile --force`, and generate
fresh docs with your changes.

### `lint`

```bash
$ npm run --prefix assets lint
```

Lint all JavaScript files using [ESLint].

### `lint:fix`

```bash
$ npm run --prefix lint:fix
```

Lint and automatically fix all JavaScript files using [ESLint].

### `test`

```bash
$ npm run --prefix assets test
```

Run all the available JavaScript tests using [Karma].

## Webpack

Internally we use [Webpack]. We also use [Less] for organizing stylesheets.

[Node.js]: https://nodejs.org/
[NPM]: https://www.npmjs.com/
[ESLint]: https://eslint.org/
[Karma]: https://karma-runner.github.io/
[Webpack]: https://webpack.js.org/
[Less]: http://lesscss.org/
