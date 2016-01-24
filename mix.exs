defmodule ExDoc.Mixfile do
  use Mix.Project

  def project do
    [app: :ex_doc,
     version: "0.11.4",
     elixir: "~> 1.0",
     elixirc_paths: elixirc_paths(Mix.env),
     deps: deps,
     aliases: aliases,
     package: package,
     source_url: "https://github.com/elixir-lang/ex_doc/",
     test_coverage: [tool: ExCoveralls],
     preferred_cli_env: [coveralls: :test],
     description: "ExDoc is a documentation generation tool for Elixir"]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    [{:earmark, "~> 0.1.17 or ~> 0.2", optional: true},
     {:markdown, github: "devinus/markdown", only: :test},
     {:cmark, "~> 0.5", only: :test},
     {:excoveralls, "~> 0.3", only: :test}]
  end

  defp elixirc_paths(:test), do: ["lib", "test/fixtures"]
  defp elixirc_paths(_),     do: ["lib"]

  defp aliases do
    [clean: [&clean_test_fixtures/1, "clean"]]
  end

  defp package do
   [licenses: ["Apache 2.0"],
    links: %{"GitHub" => "https://github.com/elixir-lang/ex_doc"}]
  end

  defp clean_test_fixtures(_args) do
    File.rm_rf "test/tmp"
  end
end
