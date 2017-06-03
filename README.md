# ExDoc

[![Build Status](https://secure.travis-ci.org/elixir-lang/ex_doc.svg?branch=master "Build Status")][build-status]
[![Coverage Status](https://coveralls.io/repos/github/elixir-lang/ex_doc/badge.svg?branch=master)][coverage-status]
[![Ebert](https://ebertapp.io/github/elixir-lang/ex_doc.svg)](https://ebertapp.io/github/elixir-lang/ex_doc)

ExDoc is a tool to generate documentation for your Elixir projects. In case you are looking for documentation for Elixir itself, [check out Elixir's website][elixir-lang].

To learn about how to document your projects check out [Elixir's writing documentation page][hex-writing-docs].

## Using ExDoc with Mix

To use ExDoc in your Mix projects, first add ExDoc as a dependency:

```elixir
def deps do
  [{:ex_doc, "~> 0.14", only: :dev, runtime: false}]
end
```

After adding ExDoc as a dependency, run `mix deps.get` to install it.

ExDoc will automatically pull in information from your projects, like the application and version. However, you may want to set `:name`, `:source_url` and `:homepage_url` to have a nicer output from ExDoc, such as:

```elixir
def project do
  [app: :my_app,
   version: "0.1.0-dev",
   deps: deps(),

   # Docs
   name: "MyApp",
   source_url: "https://github.com/USER/PROJECT",
   homepage_url: "http://YOUR_PROJECT_HOMEPAGE",
   docs: [main: "MyApp", # The main page in the docs
          logo: "path/to/logo.png",
          extras: ["README.md"]]]
end
```

Now you are ready to generate your project documentation with `mix docs`.

To see all options available when generating docs, run `mix help docs`. You may have to run `mix docs` or `mix compile` first.

## Using ExDoc via command line

You can ExDoc via the command line as follows:

1. First clone and compile it:

    ```console
    $ git clone https://github.com/elixir-lang/ex_doc.git
    $ cd ex_doc
    $ mix do deps.get, compile
    ```

2. Then you are ready to use it in your projects. First, move into your project directory and make sure it is already compiled:

    ```console
    $ cd PATH_TO_YOUR_PROJECT
    $ mix compile
    ```

3. Next invoke the ex_doc executable from your project:

    ```console
    $ PATH_TO_YOUR_EXDOC/bin/ex_doc "PROJECT_NAME" "PROJECT_VERSION" path/to/project/ebin -m "PROJECT_MODULE" -u "https://github.com/GITHUB_USER/GITHUB_REPO" -l path/to/logo.png
    ```

4. By default, ex_doc produces HTML files, but, you can also create a EPUB document passing the option `--formatter epub`:

    ```console
    $ PATH_TO_YOUR_EXDOC/bin/ex_doc "PROJECT_NAME" "PROJECT_VERSION" path/to/project/ebin -m "PROJECT_MODULE" -u "https://github.com/GITHUB_USER/GITHUB_REPO" -l path/to/logo.png -f epub
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
  * By referring to a module or function from erlang, such as (`` `:erlang` ``), ExDoc will automatically link to the Erlang documentation
  * By referring to a module, function, type or callback from any of your dependencies, such as `` `MyDep` ``, ExDoc will automatically link to that dependency documentation on [hexdocs.pm](http://hexdocs.pm/) (the link can be configured by setting `docs: [deps: [my_dep: "https://path/to/docs/"]]` in your `mix.exs`)

ExDoc supports linking to modules (`` `MyModule` ``), functions (`` `MyModule.function/1` ``), types (`` `t:MyModule.type/2` ``) and callbacks (`` `c:MyModule.callback/3` ``). If you want to link a function, type or callback in the current module, you may skip the module name, such as `` `function/1` ``.

## Changing the Markdown tool

In the examples above, we have used [Earmark][] to convert Markdown to HTML. If you prefer, you can also use hoedown (in C) or cmark (in C).

### Hoedown

Hoedown is a standard compliant Markdown parser written in C.  To use hoedown, add the elixir NIF wrapper [markdown][devinus/markdown] as a dependency to your project:

```elixir
{:markdown, github: "devinus/markdown"}
```

Update your project configuration to use hoedown:

```elixir
config :ex_doc, :markdown_processor, ExDoc.Markdown.Hoedown
```

### Cmark

[Cmark][cmark] is a CommonMark parser written in C. To use cmark add the elixir NIF wrapper [cmark.ex][cmark.ex] as a dependency to your project:

```elixir
{:cmark, "~> 0.6", only: :dev}
```

Update your project configuration to use Cmark:

```elixir
config :ex_doc, :markdown_processor, ExDoc.Markdown.Cmark
```

# License

ExDoc source code is released under Apache 2 License. The generated contents, however, are under different licenses based on projects used to help render HTML, including CSS, JS, and other assets.

Check the [LICENSE](LICENSE) file for more information.


[coverage-status]: https://coveralls.io/github/elixir-lang/ex_doc?branch=master
[build-status]: http://travis-ci.org/elixir-lang/ex_doc
[earmark]: http://github.com/pragdave/earmark
[elixir-lang]: http://elixir-lang.org/
[cmark]: https://github.com/jgm/cmark
[cmark.ex]: https://github.com/asaaki/cmark.ex
[devinus/markdown]: http://github.com/devinus/markdown
[hex-writing-docs]: https://hexdocs.pm/elixir/writing-documentation.html
