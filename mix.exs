defmodule Mix.Tasks.Compile.Sundown do
  @shortdoc "Compiles sundown that ships with ExDoc"

  def run(_) do
    Mix.shell.info System.cmd("make share/markdown.so")

    if not File.regular?("sundown/sundown") do
      IO.puts """
          If the build did not work for you, try removing the -Wl compilation flag in
          sundown/Makefile and run `mix compile` again.

          See also https://github.com/elixir-lang/ex_doc/issues/31
      """
    end
  end
end

defmodule ExDoc.Mixfile do
  use Mix.Project

  def project do
    [ app: :ex_doc,
      version: "0.1.0",
      compilers: [:sundown, :elixir, :app],
      source_url: "https://github.com/elixir-lang/ex_doc/"
    ]
  end
end
