defmodule Mix.Tasks.Compile.Sundown do
  @shortdoc "Compiles sundown that ships with ExDoc"

  def run(_) do
    if Mix.shell.cmd("make priv/markdown.so") != 0 do
      raise Mix.Error, message: "could not run `make priv/markdown.so`. Do you have make and gcc installed?"
    end
  end
end

defmodule ExDoc.Mixfile do
  use Mix.Project

  def project do
    [ app: :ex_doc,
      version: "0.1.0",
      elixir: "~> 0.11.0",
      compilers: [:sundown, :elixir, :app],
      source_url: "https://github.com/elixir-lang/ex_doc/"
    ]
  end
end
