# ExDoc

[![Build Status](https://secure.travis-ci.org/elixir-lang/ex_doc.svg?branch=master "Build Status")][build-status]
[![Coverage Status](https://coveralls.io/repos/elixir-lang/ex_doc/badge.svg?branch=master&service=github)][coverage-status]

ExDoc is a tool to generate documentation for your Elixir projects. In case you are looking for documentation for Elixir itself, [check out Elixir's website][elixir-lang].

## Using ExDoc with Mix

To use ExDoc in your Mix projects, first add ExDoc as a dependency:

```elixir
def deps do
  [{:earmark, "~> 0.1", only: :dev},
   {:ex_doc, "~> 0.11", only: :dev}]
end
```

After adding ExDoc as a dependency, run `mix deps.get` to install it.

ExDoc will automatically pull in information from your project, like the application and version. However, you may want to set `:name`, `:source_url` and `:homepage_url` to have a nicer output from ExDoc, for example:

```elixir
def project do
  [app: :repo,
   version: "0.1.0-dev",
   name: "REPO",
   source_url: "https://github.com/USER/REPO",
   homepage_url: "http://YOUR_PROJECT_HOMEPAGE",
   deps: deps,
   docs: [logo: "path/to/logo.png",
          extras: ["README.md", "CONTRIBUTING.md"]]]
end
```

Now you are ready to generate your project documentation with `mix docs`.

To see all options available when generating docs, run `mix help docs`.   You may have to do `mix docs` or `mix deps.compile` first.

## Using ExDoc via command line

You can ExDoc via the command line as follows:

1. First clone and compile it:
2. 
    ```console
    $ git clone https://github.com/elixir-lang/ex_doc.git
    $ cd ex_doc
    $ mix do deps.get, compile
    ```

2. Then you are ready to use it in your projects. First move into your project directory and ensure it is compiled:

    ```console
    $ cd PATH_TO_YOUR_PROJECT
    $ mix compile
    ```

3. Next invoke the ex_doc executable from your project:

    ```console
    $ PATH_TO_YOUR_EXDOC/bin/ex_doc "PROJECT_NAME" "PROJECT_VERSION" path/to/project/ebin -m "PROJECT_MODULE" -u "https://github.com/GITHUB_USER/GITHUB_REPO" -l path/to/logo.png
    ```

For example, here are some acceptable values:

    PROJECT_NAME    => Ecto
    PROJECT_VERSION => 0.1.0
    PROJECT_MODULE  => Ecto (the main module provided by the library)
    GITHUB_USER     => elixir-lang
    GITHUB_REPO     => ecto

## Changing the Markdown tool

In the examples above, we have used [Earmark][] to convert Markdown to HTML. If you prefer, you can also use pandoc, hoedown (in C), or cmark (in C).

### Pandoc

Install [pandoc][] using whichever means is appropriate for your system.  Odds are good it is available via whatever package manager you have available to you.

Update your project config to use pandoc:

```elixir
config :ex_doc, :markdown_processor, ExDoc.Markdown.Pandoc
```

### Hoedown

Hoedown is a standards compliant Markdown parser written in C.  To use hoedown, add the elixir NIF wrapper [markdown][devinus/markdown] as a dependency to your project:

```elixir
{:markdown, github: "devinus/markdown"}
```

Update your project config to use hoedown:

```elixir
config :ex_doc, :markdown_processor, ExDoc.Markdown.Hoedown
```

### Cmark

[Cmark][cmark] is a CommonMark parser written in C. To use cmark add the elixir NIF wrapper [cmark.ex][cmark.ex] as a dependency to your project:

```elixir
{:cmark, "~> 0.6", only: :dev}
```

Update your project config to use Cmark:

```elixir
config :ex_doc, :markdown_processor, ExDoc.Markdown.Cmark
```

# License

ExDoc source code is released under Apache 2 License. The generated contents, however, are under different licenses based on projects used to help render html, including css, js and other assets.

Check the [LICENSE](LICENSE) file for more information.


[coverage-status]: https://coveralls.io/github/elixir-lang/ex_doc?branch=master
[build-status]: http://travis-ci.org/elixir-lang/ex_doc
[earmark]: http://github.com/pragdave/earmark
[elixir-lang]: http://elixir-lang.org/
[pandoc]: http://johnmacfarlane.net/pandoc/
[cmark]: https://github.com/jgm/cmark
[cmark.ex]: https://github.com/asaaki/cmark.ex
[devinus/markdown]: http://github.com/devinus/markdown
