defmodule ExDoc.Mixfile do
  use Mix.Project

  def project do
    [
     app:        :ex_doc,
     version:    "0.5.1-dev",
     elixir:     "~> 0.14.3",
     deps:       deps,
     source_url: "https://github.com/elixir-lang/ex_doc/"
    ]
  end

  defp deps do
    [
     {:earmark, ">= 0.1.4", git: "git://github.com/pragdave/earmark", only: [:dev, :test]}
#     {:earmark, ">= 0.1.4", only: [:dev, :test]}
    ]
  end
end
