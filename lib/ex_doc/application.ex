defmodule ExDoc.Application do
  @moduledoc false
  use Application

  def start(_type, _args) do
    Makeup.Registry.register_lexer(ExDoc.ShellLexer,
      options: [],
      names: ["shell", "console", "sh", "bash", "zsh"],
      extensions: []
    )

    Enum.each([:eex, :ex_unit, :iex, :logger, :mix], &Application.load/1)
    Supervisor.start_link([ExDoc.Refs], strategy: :one_for_one)
  end
end
