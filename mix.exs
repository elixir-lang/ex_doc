defmodule ExDoc.Mixfile do
  use Mix.Project

  @source_url "https://github.com/elixir-lang/ex_doc"
  @version "0.28.0"

  def project do
    [
      app: :ex_doc,
      version: @version,
      elixir: "~> 1.11",
      deps: deps(),
      aliases: aliases(),
      package: package(),
      escript: escript(),
      elixirc_paths: elixirc_paths(Mix.env()),
      source_url: @source_url,
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [coveralls: :test],
      name: "ExDoc",
      description: "ExDoc is a documentation generation tool for Elixir",
      docs: docs()
    ]
  end

  def application do
    [
      extra_applications: [:eex, :crypto],
      mod: {ExDoc.Application, []}
    ]
  end

  defp deps do
    [
      {:earmark_parser, "~> 1.4.19"},
      {:makeup_elixir, "~> 0.14"},
      {:makeup_erlang, "~> 0.1"},
      {:jason, "~> 1.2", only: :test}
    ]
  end

  defp aliases do
    [
      build: ["cmd --cd assets npm run build", "compile --force", "docs"],
      clean: [&clean_test_fixtures/1, "clean"],
      fix: ["format", "cmd --cd assets npm run lint:fix"],
      lint: ["format --check-formatted", "cmd --cd assets npm run lint"],
      setup: ["deps.get", "cmd --cd assets npm install"]
    ]
  end

  defp package do
    [
      licenses: ["Apache-2.0"],
      maintainers: ["José Valim", "Milton Mazzarri", "Wojtek Mach"],
      files: ["formatters", "lib", "mix.exs", "LICENSE", "CHANGELOG.md", "README.md"],
      links: %{
        "GitHub" => @source_url,
        "Changelog" => "https://hexdocs.pm/ex_doc/changelog.html",
        "Writing documentation" => "https://hexdocs.pm/elixir/writing-documentation.html"
      }
    ]
  end

  defp escript do
    [
      main_module: ExDoc.CLI
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp docs do
    [
      main: "readme",
      extras: [
        "README.md",
        "LICENSE",
        "CHANGELOG.md": [filename: "CHAN.GE.LOG"]
      ],
      source_ref: "v#{@version}",
      source_url: @source_url,
      groups_for_modules: [
        Markdown: [
          ExDoc.Markdown,
          ExDoc.Markdown.Earmark
        ],
        "Formatter API": [
          ExDoc.Config,
          ExDoc.Formatter.EPUB,
          ExDoc.Formatter.HTML,
          ExDoc.Formatter.HTML.Autolink,
          ExDoc.FunctionNode,
          ExDoc.ModuleNode,
          ExDoc.TypeNode
        ]
      ],
      skip_undefined_reference_warnings_on: ["CHANGELOG.md"]
    ]
  end

  defp clean_test_fixtures(_args) do
    File.rm_rf("test/tmp")
  end
end
