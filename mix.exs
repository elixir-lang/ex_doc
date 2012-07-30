defmodule Mix.Tasks.Compile.Sundown do
  @shortdoc "Compiles sundown that ships with exdoc"

  def run(_) do
    Mix.shell.info System.cmd("make share/markdown.so")
  end
end

defmodule ExDoc.Mixfile do
  use Mix.Project

  def project do
    [ app: :ex_doc,
      version: "0.1.0",
      compilers: [:sundown, :elixir, :app] ]
  end
end