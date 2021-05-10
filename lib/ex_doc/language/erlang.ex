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
      skip: false,
      extra_callback_types: []
    }
  end

  @impl true
  def function_data(entry, module_data) do
    {{_kind, name, arity}, _anno, _signature, _doc_content, _metadata} = entry

    %{
      extra_annotations: [],
      specs: specs(name, arity, module_data),
      doc_fallback: nil,
      line_override: nil
    }
  end

  @impl true
  def callback_data(entry, _module_data) do
    {{_kind, name, arity}, _anno, _signature, _doc, _metadata} = entry

    %{
      actual_def: {name, arity},
      signature_fallback: nil
    }
  end

  @impl true
  def type_data(_entry, _spec, _module_data) do
    %{
      signature_fallback: nil
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

  defp specs(name, arity, module_data) do
    module_data.specs
    |> Map.get({name, arity}, [])
    |> Enum.map(&Code.Typespec.spec_to_quoted(name, &1))
  end
end
