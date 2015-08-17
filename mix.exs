defmodule ExDoc.Mixfile do
  use Mix.Project

  def project do
    [app:        :ex_doc,
     version:    "0.8.4",
     elixir:     "~> 1.0",
     deps:       deps,
     aliases:    aliases,
     source_url: "https://github.com/elixir-lang/ex_doc/"]
  end

  defp deps do
    [{:earmark, "~> 0.1.17 or ~> 0.2", optional: true},
     {:markdown, github: "devinus/markdown", only: [:test]}]
  end

  defp aliases do
    [clean: [&clean_test_fixtures/1, "clean"]]
  end

  defp clean_test_fixtures(_args) do
    File.rm_rf "test/tmp"
  end
end
