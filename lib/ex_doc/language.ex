defmodule ExDoc.Language do
  @moduledoc false

  @callback id() :: atom()

  @callback filter_prefix_pattern(String.t() | nil) :: String.t()

  def get(:elixir), do: ExDoc.Language.Elixir
  def get(:erlang), do: ExDoc.Language.Erlang
end
