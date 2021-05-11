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

    specs =
      case Map.fetch(module_data.specs, {name, arity}) do
        {:ok, specs} ->
          [{:attribute, 0, :spec, {{name, arity}, specs}}]

        :error ->
          []
      end

    %{
      extra_annotations: [],
      specs: specs,
      doc_fallback: nil,
      line_override: nil
    }
  end

  @impl true
  def callback_data(entry, module_data) do
    {{_kind, name, arity}, _anno, _signature, _doc, _metadata} = entry

    specs =
      case Map.fetch(module_data.callbacks, {name, arity}) do
        {:ok, specs} ->
          [{:attribute, 0, :callback, {{name, arity}, specs}}]

        :error ->
          []
      end

    %{
      actual_def: {name, arity},
      specs: specs,
      signature_fallback: nil,
      line: nil
    }
  end

  @impl true
  def type_data(_entry, spec, _module_data) do
    %{
      spec: {:attribute, 0, :type, spec},
      signature_fallback: nil
    }
  end

  @impl true
  def typespec(nil, _opts) do
    nil
  end

  def typespec(attribute, _opts) do
    {:attribute, _, type, _} = attribute

    # `-type ` => 6
    offset = byte_size(Atom.to_string(type)) + 2

    options = [linewidth: 98 + offset]
    :erl_pp.attribute(attribute, options) |> IO.iodata_to_binary() |> trim_offset(offset)
  end

  # `-type t() :: atom()` becomes `t() :: atom().`
  defp trim_offset(binary, offset) do
    binary
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn line ->
      binary_part(line, offset, byte_size(line) - offset)
    end)
    |> Enum.join("\n")
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
