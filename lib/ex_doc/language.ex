defmodule ExDoc.Language do
  @moduledoc false

  @callback id() :: atom()

  def get(:elixir), do: ExDoc.Language.Elixir
  def get(:erlang), do: ExDoc.Language.Erlang
end
