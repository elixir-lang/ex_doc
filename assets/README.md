# Assets

In this directory live all assets for `ExDoc`. The built, ready-to-use
versions are found in `formatters/{html,epub}/dist`.

To work on these assets you first need to install [Node.js] and [npm]. (npm
is usually installed along with Node.js.) The build process is currently tested
in Node 16 LTS.

Assets are built with [esbuild], which, along with the JavaScript linter and
test-runner, is set as a dependency in the assets `package.json` and installed
via [npm]:

```bash
$ npm install --prefix assets
```

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
$ npm run --prefix assets lint:fix
```

Lint and automatically fix all JavaScript files using [ESLint].

### `test`

```bash
$ npm run --prefix assets test
```

Run all the available JavaScript tests using [Karma].

[esbuild]: https://esbuild.github.io
[Node.js]: https://nodejs.org/
[npm]: https://www.npmjs.com/
[ESLint]: https://eslint.org/
[Karma]: https://karma-runner.github.io/
