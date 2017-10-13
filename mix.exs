defmodule ExDoc.Mixfile do
  use Mix.Project

  @version "0.18.1"

  def project do
    [
      app: :ex_doc,
      version: @version,
      elixir: "~> 1.3",
      deps: deps(),
      aliases: aliases(),
      package: package(),
      escript: escript(),
      source_url: "https://github.com/elixir-lang/ex_doc/",
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [coveralls: :test],
      description: "ExDoc is a documentation generation tool for Elixir",
      xref: [exclude: [Cmark, Markdown]],
      docs: docs()
   ]
  end

  def application do
    []
  end

  defp deps do
    [
      {:earmark, "~> 1.1"},
      {:cmark, "~> 0.5", only: :test},
      {:excoveralls, "~> 0.3", only: :test}
    ]
  end

  defp aliases do
    [
      clean: [&clean_test_fixtures/1, "clean"]
    ]
  end

  defp package do
   [
      licenses: ["Apache 2.0"],
      maintainers: ["José Valim", "Eksperimental", "Milton Mazzarri", "Friedel Ziegelmayer"],
      files: ["formatters", "lib", "mix.exs", "LICENSE", "CHANGELOG.md", "README.md"],
      links: %{
        "GitHub" => "https://github.com/elixir-lang/ex_doc",
        "Writing documentation" => "https://hexdocs.pm/elixir/writing-documentation.html"
      }
    ]
  end

  defp escript do
    [
      main_module: ExDoc.CLI
    ]
  end

  defp docs do
    [
      main: "readme",
      extras: ["README.md"],
      source_ref: "v#{@version}",
      source_url: "https://github.com/elixir-lang/ex_doc",
      groups_for_modules: [
        "Markdown": [
          ExDoc.Markdown,
          ExDoc.Markdown.Cmark,
          ExDoc.Markdown.Earmark,
          ExDoc.Markdown.Hoedown
        ],

        "Formatter API": [
          ExDoc.Config,
          ExDoc.Formatter.EPUB,
          ExDoc.Formatter.HTML,
          ExDoc.Formatter.HTML.Autolink,
          ExDoc.FunctionNode,
          ExDoc.ModuleNode,
          ExDoc.TypeNode,
        ]
      ]
    ]
  end

  defp clean_test_fixtures(_args) do
    File.rm_rf "test/tmp"
  end
end
