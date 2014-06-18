defmodule Mix.Tasks.Compile.Sundown do
  @shortdoc "Compiles sundown that ships with ExDoc"

  def run(_) do
    if match? {:win32, _}, :os.type do
      if Mix.shell.cmd("nmake /F Makefile.win priv\\markdown.dll") != 0 do
        raise Mix.Error, message: "could not run `nmake`. Do you have Visual Studio installed and in your Path?"
      end
    else
      if Mix.shell.cmd("make priv/markdown.so") != 0 do
        raise Mix.Error, message: "could not run `make priv/markdown.so`. Do you have make and gcc installed?"
      end
    end
  end
end

defmodule ExDoc.Mixfile do
  use Mix.Project

  def project do
    [ app: :ex_doc,
      version: "0.1.0",
      elixir: "~> 0.14.0-dev",
      compilers: [:sundown, :elixir, :app],
      source_url: "https://github.com/elixir-lang/ex_doc/"
    ]
  end
end
