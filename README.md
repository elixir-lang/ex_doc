# ExDoc

ExDoc is a tool to generate documentation for your Elixir projects. In case you are looking for documentation for Elixir itself, [check out Elixir's website](http://elixir-lang.org/).

To use ExDoc, you need Elixir master.

## Using ExDoc with Mix

To use ExDoc in your Mix projects, first add ExDoc as a dependency:

```elixir
def deps do
  [ { :ex_doc, github: "elixir-lang/ex_doc" } ]
end
```

After adding ExDoc as a dependency, please run `mix deps.get` to install it.

ExDoc will automatically pull in information from your project, like the application and version. However, you may want to set `:name`, `:source_url` and `:homepage_url` to have a nicer output from ExDoc, for example:

```elixir
def project do
  [ app: :repo
    version: "0.1.0.dev",
    name: "REPO",
    source_url: "https://github.com/USER/REPO",
    homepage_url: "http://YOUR_PROJECT_HOMEPAGE"
    deps: deps ]
end
```

Now you are ready to generate your project documentation with `mix docs`. There are other options available, you can check them out by running `mix help docs`.

## Using ExDoc via command line

You can ExDoc via the command line as follows:

1. First clone and compile it:

        git clone https://github.com/elixir-lang/ex_doc.git
        cd ex_doc
        mix compile

2. Then you are ready to use it in your projects. First move into your project directory and ensure it is compiled:

        cd PATH_TO_YOUR_PROJECT
        mix compile

3. Next invoke the ex_doc executable from your project:

        PATH_TO_YOUR_EXDOC/bin/ex_doc "PROJECT_NAME" "PROJECT_VERSION" ebin -m "PROJECT_MODULE" -u "https://github.com/GITHUB_USER/GITHUB_REPO"

For example, here are some acceptable values:

    PROJECT_NAME    => Dynamo
    PROJECT_VERSION => 0.1
    PROJECT_MODULE  => Dynamo (the main module provided by the library)
    GITHUB_USER     => elixir-lang
    GITHUB_REPO     => dynamo

# License

ExDoc source code is released under Apache 2 License with snippets under MIT-LICENSE.

Check LICENSE file for more information.
