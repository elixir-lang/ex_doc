defmodule ExDoc.Mixfile do
  use Mix.Project

  @source_url "https://github.com/elixir-lang/ex_doc"
  @version "0.31.2"

  def project do
    [
      app: :ex_doc,
      version: @version,
      elixir: "~> 1.12",
      deps: deps(),
      aliases: aliases(),
      package: package(),
      escript: escript(),
      elixirc_paths: elixirc_paths(Mix.env()),
      source_url: @source_url,
      test_elixirc_options: [docs: true, debug_info: true],
      name: "ExDoc",
      description: "ExDoc is a documentation generation tool for Elixir",
      docs: docs()
    ]
  end

  def application do
    [
      extra_applications: [:eex] ++ extra_applications(Mix.env()),
      mod: {ExDoc.Application, []}
    ]
  end

  defp extra_applications(:test), do: [:edoc, :xmerl]
  defp extra_applications(_), do: []

  defp deps do
    [
      {:earmark_parser, "~> 1.4.39"},
      {:makeup_elixir, "~> 0.14"},
      {:makeup_erlang, "~> 0.1"},
      # Add other makeup lexers as optional for the executable
      {:makeup_c, ">= 0.1.1", optional: true},
      {:makeup_html, ">= 0.0.0", only: :dev},
      {:jason, "~> 1.2", only: :test},
      {:floki, "~> 0.0", only: :test},
      {:easyhtml, "~> 0.0", only: :test}
    ]
  end

  defp aliases do
    [
      build: ["cmd --cd assets npm run build", "compile --force", "docs"],
      clean: [&clean_test_fixtures/1, "clean"],
      fix: ["format", "cmd --cd assets npm run lint:fix"],
      lint: ["format --check-formatted", "cmd --cd assets npm run lint"],
      setup: ["deps.get", "cmd mkdir -p tmp/handlebars", "cmd --cd assets npm install"]
    ]
  end

  defp package do
    [
      licenses: ["Apache-2.0"],
      maintainers: ["José Valim", "Milton Mazzarri", "Wojtek Mach"],
      files: ~w(CHANGELOG.md Cheatsheet.cheatmd formatters lib LICENSE mix.exs README.md),
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
      extras:
        [
          "README.md",
          "Cheatsheet.cheatmd",
          "CHANGELOG.md"
        ] ++ test_dev_examples(Mix.env()),
      source_ref: "v#{@version}",
      source_url: @source_url,
      groups_for_modules: [
        Markdown: [
          ExDoc.Markdown,
          ExDoc.Markdown.Earmark
        ]
      ],
      groups_for_extras: [
        Examples: ~r"test/examples"
      ],
      skip_undefined_reference_warnings_on: [
        "CHANGELOG.md"
      ]
    ]
  end

  defp test_dev_examples(:dev), do: Path.wildcard("test/examples/*")
  defp test_dev_examples(_), do: []

  defp clean_test_fixtures(_args) do
    File.rm_rf("test/tmp")
  end
end
