defmodule ExDoc.Application do
  @moduledoc false
  use Application

  def start(_type, _args) do
    Makeup.Registry.register_lexer(ExDoc.ShellLexer,
      options: [],
      names: ["shell", "console", "sh", "bash", "zsh"],
      extensions: []
    )

    # Load applications so we can find their modules in docs
    Enum.each([:eex, :ex_unit, :iex, :logger, :mix], &Application.load/1)

    # Start all applications with the makeup prefix
    for {app, _, _} <- Application.loaded_applications(),
        match?("makeup_" <> _, Atom.to_string(app)) do
      Application.ensure_all_started(app)
    end

    children = [
      ExDoc.Refs
    ]

    start_httpc()

    Supervisor.start_link(children, strategy: :one_for_one)
  end

  defp start_httpc() do
    :inets.start(:httpc, profile: :ex_doc)

    opts = [
      max_sessions: 8,
      max_keep_alive_length: 4,
      keep_alive_timeout: 120_000
    ]

    :httpc.set_options(opts, :ex_doc)
  end
end
