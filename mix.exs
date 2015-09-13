defmodule ExDoc.Mixfile do
  use Mix.Project

  def project do
    [app:               :ex_doc,
     version:           "0.9.1-dev",
     elixir:            "~> 1.0",
     deps:              deps,
     aliases:           aliases,
     package:           package,
     source_url:        "https://github.com/elixir-lang/ex_doc/",
     test_coverage:     [tool: ExCoveralls],
     preferred_cli_env: [coveralls: :test]]
  end

  defp deps do
    [{:earmark, "~> 0.1.17 or ~> 0.2", optional: true},
     {:markdown, github: "devinus/markdown", only: [:test]},
     {:excoveralls, "~> 0.3", only: [:test]}]
  end

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
