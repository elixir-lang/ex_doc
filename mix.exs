defmodule ExDoc.Mixfile do
  use Mix.Project

  def project do
    [app: :ex_doc,
     version: "0.16.2",
     elixir: "~> 1.3",
     deps: deps(),
     aliases: aliases(),
     package: package(),
     escript: escript(),
     source_url: "https://github.com/elixir-lang/ex_doc/",
     test_coverage: [tool: ExCoveralls],
     preferred_cli_env: [coveralls: :test],
     description: "ExDoc is a documentation generation tool for Elixir",
     xref: [exclude: [Cmark, Markdown]]]
  end

  def application do
    [applications: []]
  end

  defp deps do
    [{:earmark, "~> 1.1"},
     {:markdown, github: "devinus/markdown", only: :test},
     {:cmark, "~> 0.5", only: :test},
     {:excoveralls, "~> 0.3", only: :test}]
  end

  defp aliases do
    [clean: [&clean_test_fixtures/1, "clean"]]
  end

  defp package do
   [licenses: ["Apache 2.0"],
    maintainers: ["José Valim", "Eksperimental", "Milton Mazzarri", "Friedel Ziegelmayer"],
    links: %{"GitHub" => "https://github.com/elixir-lang/ex_doc",
             "Writing documentation" => "https://hexdocs.pm/elixir/writing-documentation.html"}]
  end

  def escript do
    [main_module: ExDoc.CLI]
  end

  defp clean_test_fixtures(_args) do
    File.rm_rf "test/tmp"
  end
end
