defmodule ExDoc.Mixfile do
  use Mix.Project

  def project do
    [ app: :ex_doc,
      version: "0.1.0",
      deps: deps,
      compilers: [:elixir, :app] ]
  end

  defp deps do
      [ { :markdown, "0.1.0", git: "https://github.com/erlware/erlmarkdown.git",
          compile: "../../rebar compile" } ]
  end
end
