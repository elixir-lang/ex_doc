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

  @impl true
  def module_data(module) do
    {type, skip} = module_type_and_skip(module)

    %{
      id: inspect(module),
      title: module_title(module, type),
      type: type,
      skip: skip
    }
  end

  ## Helpers

  defp module_type_and_skip(module) do
    cond do
      function_exported?(module, :__struct__, 0) and
          match?(%{__exception__: true}, module.__struct__) ->
        {:exception, false}

      function_exported?(module, :__protocol__, 1) ->
        {:protocol, false}

      function_exported?(module, :__impl__, 1) ->
        {:impl, true}

      function_exported?(module, :behaviour_info, 1) ->
        {:behaviour, false}

      match?("Elixir.Mix.Tasks." <> _, Atom.to_string(module)) ->
        {:task, false}

      true ->
        {:module, false}
    end
  end

  defp module_title(module, :task), do: "mix " <> task_name(module)
  defp module_title(module, _), do: inspect(module)

  defp task_name(module) do
    "Elixir.Mix.Tasks." <> name = Atom.to_string(module)
    name |> String.split(".") |> Enum.map_join(".", &Macro.underscore/1)
  end
end
