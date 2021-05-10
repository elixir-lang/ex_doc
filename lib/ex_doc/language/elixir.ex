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

  @impl true
  def function_data(entry, module_data) do
    {{kind, name, arity}, _anno, _signature, _doc_content, metadata} = entry

    extra_annotations =
      case {kind, name, arity} do
        {:macro, _, _} -> ["macro"]
        {_, :__struct__, 0} -> ["struct"]
        _ -> []
      end

    actual_def = actual_def(name, arity, kind)

    %{
      extra_annotations: extra_annotations,
      specs: specs(kind, name, actual_def, module_data),
      doc_fallback: fn ->
        impl = Map.fetch(module_data.impls, actual_def)

        callback_doc_ast(name, arity, impl) ||
          delegate_doc_ast(metadata[:delegate_to])
      end,
      line_override: find_function_line(module_data, actual_def)
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

  defp specs(kind, name, actual_def, module_data) do
    specs =
      module_data.specs
      |> Map.get(actual_def, [])
      |> Enum.map(&Code.Typespec.spec_to_quoted(name, &1))

    if kind == :macro do
      Enum.map(specs, &remove_first_macro_arg/1)
    else
      specs
    end
  end

  defp actual_def(name, arity, :macrocallback) do
    {String.to_atom("MACRO-" <> to_string(name)), arity + 1}
  end

  defp actual_def(name, arity, :macro) do
    {String.to_atom("MACRO-" <> to_string(name)), arity + 1}
  end

  defp actual_def(name, arity, _), do: {name, arity}

  defp remove_first_macro_arg({:"::", info, [{name, info2, [_term_arg | rest_args]}, return]}) do
    {:"::", info, [{name, info2, rest_args}, return]}
  end

  defp remove_first_macro_arg({:when, meta, [lhs, rhs]}) do
    {:when, meta, [remove_first_macro_arg(lhs), rhs]}
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

  defp find_function_line(%{abst_code: abst_code}, {name, arity}) do
    Enum.find_value(abst_code, fn
      {:function, anno, ^name, ^arity, _} -> anno_line(anno)
      _ -> nil
    end)
  end

  defp anno_line(line) when is_integer(line), do: abs(line)
  defp anno_line(anno), do: anno |> :erl_anno.line() |> abs()
end
