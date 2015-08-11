# ExDoc

[![Build Status](https://secure.travis-ci.org/elixir-lang/ex_doc.svg?branch=master "Build Status")](http://travis-ci.org/elixir-lang/ex_doc)

ExDoc is a tool to generate documentation for your Elixir projects. In case you are looking for documentation for Elixir itself, [check out Elixir's website](http://elixir-lang.org/).

## Using ExDoc with Mix

To use ExDoc in your Mix projects, first add ExDoc as a dependency:

```elixir
def deps do
  [{:earmark, "~> 0.1", only: :dev},
   {:ex_doc, "~> 0.8", only: :dev}]
end
```

After adding ExDoc as a dependency, run `mix deps.get` to install it.

ExDoc will automatically pull in information from your project, like the application and version. However, you may want to set `:name`, `:source_url` and `:homepage_url` to have a nicer output from ExDoc, for example:

```elixir
def project do
  [app: :repo
   version: "0.1.0-dev",
   name: "REPO",
   source_url: "https://github.com/USER/REPO",
   homepage_url: "http://YOUR_PROJECT_HOMEPAGE"
   deps: deps]
end
```

Now you are ready to generate your project documentation with `mix docs`.

## Using ExDoc via command line

You can ExDoc via the command line as follows:

1. First clone and compile it:

        git clone https://github.com/elixir-lang/ex_doc.git
        cd ex_doc
        mix do deps.get, compile

2. Then you are ready to use it in your projects. First move into your project directory and ensure it is compiled:

        cd PATH_TO_YOUR_PROJECT
        mix compile

3. Next invoke the ex_doc executable from your project:

        PATH_TO_YOUR_EXDOC/bin/ex_doc "PROJECT_NAME" "PROJECT_VERSION" path/to/project/ebin -m "PROJECT_MODULE" -u "https://github.com/GITHUB_USER/GITHUB_REPO"

For example, here are some acceptable values:

    PROJECT_NAME    => Dynamo
    PROJECT_VERSION => 0.1.0
    PROJECT_MODULE  => Ecto (the main module provided by the library)
    GITHUB_USER     => elixir-lang
    GITHUB_REPO     => ecto

## Changing the Markdown tool

In the examples above, we have used [Earmark](http://github.com/pragdave/earmark) to convert Markdown to HTML. If you prefer, you can also use pandoc or hoedown (in C):

  * Install [pandoc](http://johnmacfarlane.net/pandoc/) - which is available in multiple package managers and provides installers for different operating systems. Pandoc must be installed just once and it will be used for all projects;

  * To use hoedown - add http://github.com/devinus/markdown as a dependency to your project as: `{:markdown, github: "devinus/markdown"}`

Then add the entry:

    config :ex_doc, :markdown_processor, ExDoc.Markdown.Pandoc  # or ExDoc.Markdown.Hoedown

to your `config/config.exs` file.

To see all options available when generating docs, just run `mix help docs`.

# License

ExDoc source code is released under Apache 2 License. The generated contents, however, are under different licenses based on projects used to help render html, including css, js and other assets.

Check the [LICENSE](LICENSE) file for more information.
