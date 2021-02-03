defmodule ExDoc.Language.Erlang do
  @moduledoc false

  @behaviour ExDoc.Language

  @impl true
  def filter_prefix_pattern(filter_prefix) do
    "#{filter_prefix}*.beam"
  end

  @impl true
  def module_type(module) do
    cond do
      function_exported?(module, :behaviour_info, 1) ->
        :behaviour

      true ->
        :module
    end
  end

  @impl true
  def skip_module?(_) do
    false
  end

  @impl true
  def skip_module_type?(_) do
    false
  end

  @impl true
  def actual_def(_kind, name, arity) do
    {name, arity}
  end

  @impl true
  def extra_annotations(_kind, _name, _arity) do
    []
  end

  @impl true
  def normalize_specs(specs, _type, name, arity) do
    specs
    |> Map.get({name, arity}, [])
    |> Enum.map(&Code.Typespec.spec_to_quoted(name, &1))
  end

  @impl true
  def module_title_and_id(module, _type) do
    id = to_string(module)
    {id, id}
  end

  @impl true
  def doc_fallback(_name, _arity, _impl, _metadata) do
    nil
  end
end
