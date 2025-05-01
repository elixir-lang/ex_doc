defmodule ExDoc.CopyDocUtils do
  alias ExDoc.DocAST

  def copy_doc_info(metadata, {doc_file, doc_line} = _dbg_info \\ {__ENV__.file, __ENV__.line}) do
    delegate_to = metadata[:delegate_to]

    case metadata[:copy] do
      copy when is_boolean(copy) and copy == true ->
        if delegate_to == nil do
          ExDoc.Utils.warn("no `delegate_to` specified",
            file: doc_file,
            line: doc_line
          )
          nil
        else
          delegate_to
        end
      {_m, _f, _a} = copy -> copy
      _ -> nil
    end
  end

  def extract_doc({m, f, a}, config) do
    case get_docs(m, [:function]) do
      {nil, nil, nil} ->
        nil

      {_language, format, docs} ->
        case extract_function_doc(docs, f, a) do
          nil ->
            nil

          :none ->
            nil

          %{"en" => doc} ->
            module = remove_leading_elixir(m)
            function = Atom.to_string(f)

            # IO.puts("doc: " <> inspect doc)
            rewrite_doc(doc, {m, f, a})
            |> config.copy_doc_decorator.({module, function, a})
            |> DocAST.parse!(format, [])
        end
    end
  end

  @doc """
  Extract docs from a module, filtered by the kinds (list of :function, :type, ...)
  """
  # TODO: see whether we can merge this with Retriever.get_docs, because they are similar
  def get_docs(module, kinds) do
    case Code.fetch_docs(module) do
      {:docs_v1, _number, language, format, _module_doc, _donno, docs} ->
        docs =
          for {{kind, _name, _arity}, _info, _type, _txt, _donno} = doc <- docs,
              kind in kinds,
              do: doc

        {language, format, docs}

      {:error, _msg} ->
        {nil, nil, nil}
    end
  end

  def extract_function_doc(docs, function, arity) do
    case Enum.find(docs, nil, fn {{_type, func_name, func_arity}, _info, _types, _doc, _opts} ->
           func_name === function && func_arity === arity
         end) do
      nil ->
        nil

      {_def, _info, _types, doc, _opts} ->
        doc
    end
  end

  def rewrite_doc(doc, mfa) do
    # we don't need to rewrite code blocks
    Regex.replace(
      ~r/[`]([^`]+)[`]/,
      doc,
      fn _full, ref ->
        "`#{rewrite_ref(ref, mfa)}`"
      end,
      global: true
    )
  end

  def rewrite_ref(ref, {module, _function, _arity}) do
    parts =
      Regex.named_captures(
        ~r/(?<type>[ct]:){0,1}(?:Elixir\.)?(?<module>(?:[^.]*[.])*)?(?<func>.*)\/(?<arity>\d+)/,
        ref
      )

    if parts != nil do
      parts
      |> cleanup()
      |> build_new_ref(module)
    else
      ref
    end
  end

  defp cleanup(%{"type" => type, "module" => module, "func" => func, "arity" => arity}) do
    %{
      type: type,
      module: String.trim(module) |> String.replace_trailing("\.", ""),
      func: func,
      arity: Integer.parse(arity) |> elem(0)
    }
  end

  @spec build_new_ref(%{
    type: String.t(),
    module: String.t(),
    func: String.t(),
    arity: non_neg_integer()
    }, module()) :: String.t()
  def build_new_ref(%{
      type: ref_type,
      module: ref_module,
      func: ref_func,
      arity: ref_arity
      }, module) do
    empty_ref_module =  String.length(ref_module) == 0
    # is_ref_module_func = is_module_ref?(source_module, ref_func, ref_arity)
    is_module_func = is_module_ref?(module, ref_func, ref_arity)

    ref_module =
        # NOTE: we could stop the rewriting of a ref IF the function exists
        #       in the calling module. However that function might not be
        #       a delegated function and therefore would be wrong. We play it safe
        #       and ALWAYS add the module name to which we delegate.
        case {empty_ref_module, is_module_func} do
        {true, true} -> remove_leading_elixir(module)
        _ -> if String.length(ref_module) > 0, do: ref_module <> ".", else: ""
      end

    "#{ref_type}#{ref_module}#{ref_func}/#{ref_arity}"
  end

  def is_module_ref?(module, func, arity) do
    func = String.to_atom(func)

    function_exported?(module, func, arity) or
      callback_func?(module, func, arity) or
      type_func(module, func, arity)
  end

  defp type_func(module, func, arity) do
    {_lang, _format, funcs} = get_docs(module, [:type])

    Enum.filter(funcs, fn {{this_type, this_func, this_arity}, _, _, _, _} ->
      {this_type, this_func, this_arity} == {:type, func, arity}
    end)
    |> length() > 0
  end

  defp callback_func?(module, func, arity) do
    if function_exported?(module, :behaviour_info, 1) do
      callbacks = module.behaviour_info(:callbacks)
      {func, arity} in callbacks
    else
      false
    end
  end

  def remove_leading_elixir(module) do
    String.replace_leading(Atom.to_string(module), "Elixir.", "") <> "."
  end
end
