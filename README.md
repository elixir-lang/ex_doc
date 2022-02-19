# ExDoc

[![Build Status](https://github.com/elixir-lang/ex_doc/workflows/CI/badge.svg)](https://github.com/elixir-lang/ex_doc/actions?query=workflow%3A%22CI%22)
[![Coverage Status](https://coveralls.io/repos/github/elixir-lang/ex_doc/badge.svg?branch=master)](https://coveralls.io/github/elixir-lang/ex_doc?branch=master)

ExDoc is a tool to generate documentation for your Elixir projects. To see an example, [you can access Elixir's official docs](https://hexdocs.pm/elixir/).

To learn about how to document your projects, see [Elixir's writing documentation page](https://hexdocs.pm/elixir/writing-documentation.html).

To see all supported options, see the documentation for [mix docs](https://hexdocs.pm/ex_doc/Mix.Tasks.Docs.html).

## Features

ExDoc ships with many features:

  * Automatically generates HTML and EPUB documents from your API documentation
  * Responsive design with built-in layout for phones and tablets
  * Support for custom pages, guides, and livebooks (in addition to the API reference)
  * Support for custom grouping of modules, functions, and pages in the sidebar
  * Generates HTML documentation accessible online and offline
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

If you are using Elixir v1.10, or later:

```elixir
def deps do
  [
    {:ex_doc, "~> 0.27", only: :dev, runtime: false},
  ]
end
```

> Note: if you are using Elixir v1.7, v1.8, or v1.9, use `~> 0.22.0`.

After adding ExDoc as a dependency, run `mix deps.get` to install it.

> Note: Some Operating System distributions split Erlang into multiple packages and at least one ExDoc dependency (`earmark_parser`) requires Erlang development environment. If you get a message like "/usr/lib/erlang/lib/parsetools-2.3.1/include/yeccpre.hrl: no such file or directory", it means you lack this environment. For instance, on the Debian operating system and its derivatives, you need to `apt install erlang-dev`.

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

3. Next invoke the `ex_doc` executable from your project:

   ```bash
   $ ex_doc "PROJECT_NAME" "PROJECT_VERSION" _build/dev/lib/project/ebin -m "PROJECT_MODULE" -u "https://github.com/GITHUB_USER/GITHUB_REPO" -l path/to/logo.png
   ```

For example, here are some acceptable values:

    PROJECT_NAME    => Ecto
    PROJECT_VERSION => 0.1.0
    PROJECT_MODULE  => Ecto (the main module provided by the library)
    GITHUB_USER     => elixir-lang
    GITHUB_REPO     => ecto

## Using ExDoc with Erlang projects

From Erlang/OTP 24+, you can use ExDoc to render your Erlang documentation written with EDoc. See [`rebar3_ex_doc`](https://github.com/starbelly/rebar3_ex_doc/) for more information.

## Metadata

ExDoc supports certain metadata keys in your documentation. For example, the `since` metadata is used to annotate from when a given module/function is available. In Elixir, you can add metadata to modules and functions, respectively, like this:

```elixir
@moduledoc since: "1.10.0"
@doc since: "1.13.1"
```

In Erlang's EDoc, you would do:

```erlang
%% @since 0.1.0
```

The following metadata is available for both modules and functions:

  * `deprecated` (string) - marks the given module/function as deprecated with the given string as reason
  * `since` (string) - annotates the given module/function is available from a particular version

The following metadata is available for modules:

  * `tags` (list of atoms) - a list of strings to be added as tags to the module (not supported by EDoc)

## Auto-linking

ExDoc for Elixir will automatically generate links across modules and functions if you enclose them in backticks:

  * By referring to a module, function, type or callback from your project, such as `` `MyModule` ``, ExDoc will automatically link to those
  * By referring to a module, function, type or callback from Elixir, such as `` `String` ``, ExDoc will automatically link to Elixir's stable documentation
  * By referring to a function, type, or callback from OTP, such as (`` `:queue.new/0` ``), ExDoc will automatically link to the OTP documentation
  * By referring to a module, function, type or callback from any of your dependencies, such as `` `MyDep` ``, ExDoc will automatically link to that dependency documentation on [hexdocs.pm](https://hexdocs.pm/) (the link can be configured by setting `docs: [deps: [my_dep: "https://path/to/docs/"]]` in your `mix.exs`)

ExDoc supports linking to modules (`` `MyModule` ``), functions (`` `MyModule.function/1` ``), types (`` `t:MyModule.type/2` ``) and callbacks (`` `c:MyModule.callback/3` ``). If you want to link a function, type or callback in the current module, you may skip the module name, such as `` `function/1` ``.

You can also use a custom text, e.g.: `` [custom text](`MyModule.function/1`) ``. This also allows to refer to OTP modules, e.g.: `` [`:array`](`:array`) ``.

Link to extra pages like this: `` [Up and running](Up and running.md) `` (skipping the directory
the page is in), the final link will be automatically converted to `up-and-running.html`.

## Extensions

ExDoc renders Markdown content for you, but you can extend it to render complex objects on the page using JavaScript. To inject custom JavaScript into every page, add this to your configuration:

```elixir
docs: [
  # ...
  before_closing_body_tag: &before_closing_body_tag/1
]

# ...

defp before_closing_body_tag(:html) do
  """
  <!-- HTML injected at the end of the <body> element -->
  """
end

defp before_closing_body_tag(_), do: ""
```

### Rendering Math

If you write TeX-style math in your Markdown (like `$\sum_{i}^{N} x_i$`), they end up as raw text on the generated pages. To render them we recommend using [KaTeX](https://katex.org/), a JavaScript library that turns those expressions into actual graphics. To load and trigger KaTeX on every documentation page we can insert the following HTML:

```html
<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.13.19/dist/katex.min.css" integrity="sha384-beuqjL2bw+6DBM2eOpr5+Xlw+jiH44vMdVQwKxV28xxpoInPHTVmSvvvoPq9RdSh" crossorigin="anonymous">
<script defer src="https://cdn.jsdelivr.net/npm/katex@0.13.19/dist/katex.min.js" integrity="sha384-aaNb715UK1HuP4rjZxyzph+dVss/5Nx3mLImBe9b0EW4vMUkc1Guw4VRyQKBC0eG" crossorigin="anonymous"></script>
<script defer src="https://cdn.jsdelivr.net/npm/katex@0.13.19/dist/contrib/auto-render.min.js" integrity="sha384-+XBljXPPiv+OzfbB3cVmLHf4hdUFHlWNZN5spNQ7rmHTXpd7WvJum6fIACpNNfIR" crossorigin="anonymous"
    onload="renderMathInElement(document.body);"></script>
```

For more details and configuration options see the [KaTeX Auto-render Extension](https://katex.org/docs/autorender.html).

### Rendering Vega-Lite plots

Other objects you may want to render in a special manner are code snippets. For example, assuming your Markdown includes Vega-Lite specification in `vega-lite` code snippets, you can do:

```html
<script src="https://cdn.jsdelivr.net/npm/vega@5.20.2"></script>
<script src="https://cdn.jsdelivr.net/npm/vega-lite@5.1.1"></script>
<script src="https://cdn.jsdelivr.net/npm/vega-embed@6.18.2"></script>
<script>
  document.addEventListener("DOMContentLoaded", function () {
    for (const codeEl of document.querySelectorAll("pre code.vega-lite")) {
      try {
        const preEl = codeEl.parentElement;
        const spec = JSON.parse(codeEl.textContent);
        const plotEl = document.createElement("div");
        preEl.insertAdjacentElement("afterend", plotEl);
        vegaEmbed(plotEl, spec);
        preEl.remove();
      } catch (error) {
        console.log("Failed to render Vega-Lite plot: " + error)
      }
    }
  });
</script>
```

For more details and configuration options see [vega/vega-embed](https://github.com/vega/vega-embed).

### Rendering Mermaid graphs

Similarly to the example above, if your Markdown includes Mermaid graph specification in `mermaid` code snippets, you can do:

```html
<script src="https://cdn.jsdelivr.net/npm/mermaid@8.13.3/dist/mermaid.min.js"></script>
<script>
  document.addEventListener("DOMContentLoaded", function () {
    mermaid.initialize({ startOnLoad: false });
    let id = 0;
    for (const codeEl of document.querySelectorAll("pre code.mermaid")) {
      const preEl = codeEl.parentElement;
      const graphDefinition = codeEl.textContent;
      const graphEl = document.createElement("div");
      const graphId = "mermaid-graph-" + id++;
      mermaid.render(graphId, graphDefinition, function (svgSource, bindListeners) {
        graphEl.innerHTML = svgSource;
        bindListeners && bindListeners(graphEl);
        preEl.insertAdjacentElement("afterend", graphEl);
        preEl.remove();
      });
    }
  });
</script>
```

For more details and configuration options see the [Mermaid usage docs](https://mermaid-js.github.io/mermaid/#/usage).

## Contributing

The easiest way to test changes to ExDoc is to locally rebuild the app and its own documentation:

  1. Run `mix setup` to install all dependencies
  2. Run `mix build` to generate docs. This is a custom alias that will build assets, recompile ExDoc, and output fresh docs into the `doc/` directory
  3. If you want to contribute a pull request, please do not add to your commits the files generated in the `assets/` and `formatters/` folders
  4. Run `mix lint` to check if the Elixir and JavaScript files are properly formatted.
     You can run `mix fix` to let the JavaScript linter and Elixir formatter fix the code automatically before submitting your pull request

## License

ExDoc source code is released under [Apache 2 License](LICENSE). The generated contents, however, are under different licenses based on projects used to help render HTML, including CSS, JS, and other assets.

Any documentation generated by ExDoc, or any documentation generated by any "Derivative Works" (as specified in the Apache 2 License), must include a direct, readable, and visible link to the [ExDoc repository](https://github.com/elixir-lang/ex_doc) on each rendered material. For HTML pages, a rendered material represents every single page. For PDF, EPUB and other ebook formats, it means one entry for the whole material.
