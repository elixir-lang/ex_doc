defmodule ExDoc.Application do
  @moduledoc false
  use Application

  def start(_type, _args) do
    Enum.each([:eex, :ex_unit, :iex, :logger, :mix], &Application.load/1)
    Supervisor.start_link([ExDoc.Refs], strategy: :one_for_one)
  end
end
