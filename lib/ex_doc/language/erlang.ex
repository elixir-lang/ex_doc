defmodule ExDoc.Language.Erlang do
  @moduledoc false

  @behaviour ExDoc.Language

  @impl true
  def id(), do: :erlang

  @impl true
  def filter_prefix_pattern(filter_prefix) do
    "#{filter_prefix}*.beam"
  end
end
