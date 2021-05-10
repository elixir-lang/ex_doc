defmodule ExDoc.Language.Elixir do
  @moduledoc false

  @behaviour ExDoc.Language

  @impl true
  def id(), do: :elixir

  @impl true
  def filter_prefix_pattern(filter_prefix) do
    if filter_prefix do
      "Elixir.#{filter_prefix}*.beam"
    else
      "*.beam"
    end
  end
end
