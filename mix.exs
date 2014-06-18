defmodule ExDoc.Mixfile do
  use Mix.Project

  def project do
    [app: :ex_doc,
     version: "0.5.0-dev",
     elixir: "~> 0.14.0",
     deps: deps,
     source_url: "https://github.com/elixir-lang/ex_doc/"]
  end

  defp deps do
    [{:markdown, github: "devinus/markdown", only: [:dev, :test]}]
  end
end
