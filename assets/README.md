# Assets

All asset sources for `ExDoc` live in this directory. The built, ready-to-use versions are found in `formatters/{html,epub}/dist`.

To work on these assets you need to have [Node.js] and [npm] installed. (npm is usually installed along with Node.js.) The build process is currently tested in Node 18 LTS.

Assets are built with [esbuild], which, along with the JavaScript linter and test-runner, is set as a dependency in the assets `package.json` and installed via [npm]:

```bash
$ npm install --prefix assets
```

## `npm run` scripts

The following scripts are available from the root folder of the project.

### `build`

```bash
$ npm run --prefix assets build
```

Build a complete production bundle, including JavaScript and CSS.

(Note that this is not required to be manually run when generating docs: if you run `mix build` at the `ExDoc` root after changing your assets, the assets will be recompiled, `mix compile --force` will be invoked, and fresh docs with your changes will be generated.)

### `build:watch`

```bash
$ npm run --prefix assets build:watch
```

Run the `build` command with watch mode set, providing for automatic assets rebuilds on every asset file change.

Additionally, in watch mode, the docs are built after every asset rebuild, meaning the only action required to check results after changing asset sources is to refresh/reload the browser or EPUB reader.

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
