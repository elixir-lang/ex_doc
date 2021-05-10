defmodule ExDoc.Language.Erlang do
  @moduledoc false

  @behaviour ExDoc.Language

  @impl true
  def id(), do: :erlang

  @impl true
  def filter_prefix_pattern(filter_prefix) do
    "#{filter_prefix}*.beam"
  end

  @impl true
  def module_data(module) do
    ":" <> id = inspect(module)

    %{
      id: id,
      title: id,
      type: module_type(module),
      skip: false
    }
  end

  ## Helpers

  defp module_type(module) do
    cond do
      function_exported?(module, :behaviour_info, 1) ->
        :behaviour

      true ->
        :module
    end
  end
end
