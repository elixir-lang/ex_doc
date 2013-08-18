defmodule Mix.Tasks.Compile.Sundown do
  @shortdoc "Compiles sundown that ships with ExDoc"

  def run(_) do
    Mix.shell.info System.cmd("make share/markdown.so")
  end
end

defmodule ExDoc.Mixfile do
  use Mix.Project

  def project do
    [ app: :ex_doc,
      version: "0.1.0",
      elixir: "~> 0.10.2-dev",
      compilers: [:sundown, :elixir, :app],
      source_url: "https://github.com/elixir-lang/ex_doc/"
    ]
  end
end
