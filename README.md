# ExDoc

[![Build Status](https://github.com/elixir-lang/ex_doc/workflows/CI/badge.svg)](https://github.com/elixir-lang/ex_doc/actions?query=workflow%3A%22CI%22)
[![Coverage Status](https://coveralls.io/repos/github/elixir-lang/ex_doc/badge.svg?branch=master)](https://coveralls.io/github/elixir-lang/ex_doc?branch=master)

ExDoc is a tool to generate documentation for your Elixir projects. To see an example, [you can access Elixir's official docs](https://hexdocs.pm/elixir/).

To learn about how to document your projects, see [Elixir's writing documentation page](https://hexdocs.pm/elixir/writing-documentation.html).

To see all supported options, see the documentation for [mix docs](https://hexdocs.pm/ex_doc/Mix.Tasks.Docs.html)

## Features

ExDoc ships with many features:

  * Automatically generates HTML and EPUB documents from your API documentation
  * Support for custom pages and guides (in addition to the API reference)
  * Support for custom grouping of modules, functions, and pages in the sidebar
  * Generates HTML documentation accessible online and offline
  * Responsive design with built-in layout for phones and tablets
  * Customizable logo on the generated documentation
  * Each documented entry contains a direct link back to the source code
  * Full-text search
  * Keyboard shortcuts (press `?` inside an existing documentation to bring the help dialog)
  * Quick search with autocompletion support (`s` keyboard shortcut)
  * Go-to shortcut to take to any HexDocs package documentation with autocomplete support (`g` keyboard shortcut)
  * Support for night-mode (automatically detected according to the browser preferences)
  * Show tooltips when mousing over a link to a module/function (works for the current project and across projects)
  * A version dropdown to quickly switch to other versions (automatically configured when hosted on HexDocs)

## Using ExDoc with Mix

To use ExDoc in your Mix projects, first add ExDoc as a dependency.

If you are using Elixir v1.7 and later:

```elixir
def deps do
  [
    {:ex_doc, "~> 0.22", only: :dev, runtime: false},
  ]
end
```

If you are using Elixir v1.6 and earlier:

```elixir
def deps do
  [
    {:ex_doc, "~> 0.18.0", only: :dev, runtime: false},
  ]
end
```

After adding ExDoc as a dependency, run `mix deps.get` to install it.

ExDoc will automatically pull in information from your projects, like the application and version. However, you may want to set `:name`, `:source_url` and `:homepage_url` to have a nicer output from ExDoc, such as:

```elixir
def project do
  [
    app: :my_app,
    version: "0.1.0-dev",
    deps: deps(),

    # Docs
    name: "MyApp",
    source_url: "https://github.com/USER/PROJECT",
    homepage_url: "http://YOUR_PROJECT_HOMEPAGE",
    docs: [
      main: "MyApp", # The main page in the docs
      logo: "path/to/logo.png",
      extras: ["README.md"]
    ]
  ]
end
```

Now you are ready to generate your project documentation with `mix docs`. To see all options available when generating docs, run `mix help docs`.

## Using ExDoc via command line

You can ExDoc via the command line as follows:

1. Install ExDoc as an escript:

    ```bash
    $ mix escript.install hex ex_doc
    ```

2. Then you are ready to use it in your projects. First, move into your project directory and make sure it is already compiled:

    ```bash
    $ cd PATH_TO_YOUR_PROJECT
    $ mix compile
    ```

3. Next invoke the ex_doc executable from your project:

    ```bash
    $ ex_doc "PROJECT_NAME" "PROJECT_VERSION" path/to/project/ebin -m "PROJECT_MODULE" -u "https://github.com/GITHUB_USER/GITHUB_REPO" -l path/to/logo.png
    ```

For example, here are some acceptable values:

    PROJECT_NAME    => Ecto
    PROJECT_VERSION => 0.1.0
    PROJECT_MODULE  => Ecto (the main module provided by the library)
    GITHUB_USER     => elixir-lang
    GITHUB_REPO     => ecto

## Auto-linking

ExDoc will automatically generate links across modules and functions if you enclose them in backticks:

  * By referring to a module, function, type or callback from your project, such as `` `MyModule` ``, ExDoc will automatically link to those
  * By referring to a module, function, type or callback from Elixir, such as `` `String` ``, ExDoc will automatically link to Elixir's stable documentation
  * By referring to a function, type, or callback from OTP, such as (`` `:queue.new/0` ``), ExDoc will automatically link to the OTP documentation
  * By referring to a module, function, type or callback from any of your dependencies, such as `` `MyDep` ``, ExDoc will automatically link to that dependency documentation on [hexdocs.pm](http://hexdocs.pm/) (the link can be configured by setting `docs: [deps: [my_dep: "https://path/to/docs/"]]` in your `mix.exs`)

ExDoc supports linking to modules (`` `MyModule` ``), functions (`` `MyModule.function/1` ``), types (`` `t:MyModule.type/2` ``) and callbacks (`` `c:MyModule.callback/3` ``). If you want to link a function, type or callback in the current module, you may skip the module name, such as `` `function/1` ``.

You can also use a custom text, e.g.: `` [custom text](`MyModule.function/1`) ``. This also allows to refer to OTP modules, e.g.: `` [`:array`](`:array`) ``.

Link to extra pages like this: `` [Up and running](Up and running.md) `` (skipping the directory
the page is in), the final link will be automatically converted to `up-and-running.html`.

## Contributing

The easiest way to test changes to ExDoc is to locally rebuild the app and its own documentation:

  1. Run `mix setup` to install all dependencies
  2. Run `mix build` to generate docs. This is a custom alias that will build assets, recompile ExDoc, and output fresh docs into the `doc/` directory
  3. If you want to contribute a pull request, please do not add to your commits the files generated in the `assets/` and `formatters/` folders
  4. Run `mix lint.` to check if the Elixir and JavaScript files are properly formatted.
     You can run `mix fix` to let the JavaScript linter and Elixir formatter fix the code automatically before submitting your pull request

## License

ExDoc source code is released under Apache 2 License. The generated contents, however, are under different licenses based on projects used to help render HTML, including CSS, JS, and other assets.

Check the [LICENSE](LICENSE) file for more information. Any documentation generated by ExDoc, or any documentation generated by any "Derivative Works" (as specified in the Apache 2 License), must include a direct link to [ExDoc repository](https://github.com/elixir-lang/ex_doc) on every page.
