defmodule ExDoc.Language.Elixir do
  @moduledoc false

  @behaviour ExDoc.Language

  @impl true
  def filter_prefix_pattern(filter_prefix) do
    if filter_prefix do
      "Elixir.#{filter_prefix}*.beam"
    else
      "*.beam"
    end
  end

  @impl true
  def module_type(module) do
    cond do
      function_exported?(module, :__struct__, 0) and
          match?(%{__exception__: true}, module.__struct__) ->
        :exception

      function_exported?(module, :__protocol__, 1) ->
        :protocol

      function_exported?(module, :__impl__, 1) ->
        :impl

      function_exported?(module, :behaviour_info, 1) ->
        :behaviour

      match?("Elixir.Mix.Tasks." <> _, Atom.to_string(module)) ->
        :task

      true ->
        :module
    end
  end

  @impl true
  def skip_module?(:elixir_bootstrap), do: true
  def skip_module?(Elixir), do: true
  def skip_module?(_), do: false

  @impl true
  def skip_module_type?(:impl), do: true
  def skip_module_type?(_), do: false

  @impl true
  def actual_def(:macrocallback, name, arity) do
    {String.to_atom("MACRO-" <> to_string(name)), arity + 1}
  end

  def actual_def(:macro, name, arity) do
    {String.to_atom("MACRO-" <> to_string(name)), arity + 1}
  end

  def actual_def(_, name, arity), do: {name, arity}

  @impl true
  def extra_annotations(type, name, arity) do
    case {type, name, arity} do
      {:macro, _, _} -> ["macro"]
      {_, :__struct__, 0} -> ["struct"]
      _ -> []
    end
  end

  @impl true
  def normalize_specs(specs, type, name, arity) do
    actual_def = actual_def(type, name, arity)

    specs =
      specs
      |> Map.get(actual_def, [])
      |> Enum.map(&Code.Typespec.spec_to_quoted(name, &1))

    if type == :macro do
      Enum.map(specs, &remove_first_macro_arg/1)
    else
      specs
    end
  end

  @impl true
  def module_title_and_id(module, :task) do
    {"mix " <> task_name(module), module_id(module)}
  end

  def module_title_and_id(module, _) do
    id = module_id(module)
    {id, id}
  end

  @impl true
  def doc_fallback(name, arity, impl, metadata) do
    callback_doc_ast(name, arity, impl) ||
      delegate_doc_ast(metadata[:delegate_to])
  end

  ## Internal

  defp remove_first_macro_arg({:"::", info, [{name, info2, [_term_arg | rest_args]}, return]}) do
    {:"::", info, [{name, info2, rest_args}, return]}
  end

  defp remove_first_macro_arg({:when, meta, [lhs, rhs]}) do
    {:when, meta, [remove_first_macro_arg(lhs), rhs]}
  end

  defp module_id(module) do
    case inspect(module) do
      ":" <> inspected -> inspected
      inspected -> inspected
    end
  end

  defp task_name(module) do
    "Elixir.Mix.Tasks." <> name = Atom.to_string(module)

    name
    |> String.split(".")
    |> Enum.map_join(".", &Macro.underscore/1)
  end

  defp delegate_doc_ast({m, f, a}) do
    [
      {:p, [], ["See ", {:code, [class: "inline"], [Exception.format_mfa(m, f, a)], %{}}, "."],
       %{}}
    ]
  end

  defp delegate_doc_ast(nil) do
    nil
  end

  # TODO: this could be useful for Erlang too but we'd need to generate different ref in AST.
  defp callback_doc_ast(name, arity, {:ok, behaviour}) do
    [
      {:p, [],
       [
         "Callback implementation for ",
         {:code, [class: "inline"], ["c:#{inspect(behaviour)}.#{name}/#{arity}"], %{}},
         "."
       ], %{}}
    ]
  end

  defp callback_doc_ast(_, _, _) do
    nil
  end
end
