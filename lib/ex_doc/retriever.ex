defmodule ExDoc.Retriever do
  # Functions to extract documentation information from modules.
  @moduledoc false

  defmodule Error do
    @moduledoc false
    defexception [:message]
  end

  alias ExDoc.GroupMatcher
  alias ExDoc.Retriever.Error

  @doc """
  Extract documentation from all modules in the specified directory or directories.
  """
  @spec docs_from_dir(Path.t() | [Path.t()], ExDoc.Config.t()) :: [ExDoc.ModuleNode.t()]
  def docs_from_dir(dir, config) when is_binary(dir) do
    pattern = if config.filter_prefix, do: "Elixir.#{config.filter_prefix}*.beam", else: "*.beam"
    files = Path.wildcard(Path.expand(pattern, dir))
    docs_from_files(files, config)
  end

  def docs_from_dir(dirs, config) when is_list(dirs) do
    Enum.flat_map(dirs, &docs_from_dir(&1, config))
  end

  @doc """
  Extract documentation from all modules in the specified list of files
  """
  @spec docs_from_files([Path.t()], ExDoc.Config.t()) :: [ExDoc.ModuleNode.t()]
  def docs_from_files(files, config) when is_list(files) do
    files
    |> Enum.map(&filename_to_module(&1))
    |> docs_from_modules(config)
  end

  @doc """
  Extract documentation from all modules in the list `modules`
  """
  @spec docs_from_modules([atom], ExDoc.Config.t()) :: [ExDoc.ModuleNode.t()]
  def docs_from_modules(modules, config) when is_list(modules) do
    modules
    |> Enum.flat_map(&get_module(&1, config))
    |> Enum.sort_by(fn module ->
      {GroupMatcher.group_index(config.groups_for_modules, module.group), module.id}
    end)
  end

  defp filename_to_module(name) do
    name = Path.basename(name, ".beam")
    String.to_atom(name)
  end

  # Get all the information from the module and compile
  # it. If there is an error while retrieving the information (like
  # the module is not available or it was not compiled
  # with --docs flag), we raise an exception.
  defp get_module(module, config) do
    unless Code.ensure_loaded?(module) do
      raise Error, "module #{inspect(module)} is not defined/available"
    end

    if docs_chunk = docs_chunk(module) do
      [generate_node(module, docs_chunk, config)]
    else
      []
    end
  end

  # Special case required for Elixir
  defp docs_chunk(:elixir_bootstrap), do: false

  defp docs_chunk(module) do
    unless function_exported?(Code, :fetch_docs, 1) do
      raise Error,
            "ExDoc 0.19+ requires Elixir v1.7 and later. " <>
              "For earlier Elixir versions, make sure to depend on {:ex_doc, \"~> 0.18.0\"}"
    end

    if function_exported?(module, :__info__, 1) do
      case Code.fetch_docs(module) do
        {:docs_v1, _, _, _, :hidden, _, _} ->
          false

        {:docs_v1, _, _, _, _, _, _} = docs ->
          docs

        {:error, reason} ->
          raise Error,
                "module #{inspect(module)} was not compiled with flag --docs: #{inspect(reason)}"
      end
    else
      false
    end
  end

  defp generate_node(module, docs_chunk, config) do
    source_url = config.source_url_pattern
    source_path = source_path(module, config)
    source = %{url: source_url, path: source_path}

    module_data = get_module_data(module, docs_chunk)
    {doc_line, moduledoc, metadata} = get_module_docs(module_data)
    line = find_module_line(module_data) || doc_line

    docs = get_docs(module_data, source) ++ get_callbacks(module_data, source)
    types = get_types(module_data, source)
    {title, id} = module_title_and_id(module_data)
    module_group = GroupMatcher.match_module(config.groups_for_modules, module, id)

    %ExDoc.ModuleNode{
      id: id,
      title: title,
      module: module_data.name,
      group: module_group,
      type: module_data.type,
      deprecated: metadata[:deprecated],
      docs: Enum.sort_by(docs, & &1.id),
      doc: moduledoc,
      doc_line: doc_line,
      typespecs: Enum.sort_by(types, & &1.id),
      source_path: source_path,
      source_url: source_link(source, line)
    }
  end

  # Module Helpers

  defp get_module_data(module, docs_chunk) do
    %{
      name: module,
      type: get_type(module),
      specs: get_specs(module),
      impls: get_impls(module),
      abst_code: get_abstract_code(module),
      docs: docs_chunk
    }
  end

  defp get_type(module) do
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

  defp get_module_docs(%{docs: docs}) do
    case docs do
      {:docs_v1, anno, _, _, %{"en" => doc}, metadata, _} -> {anno_line(anno), doc, metadata}
      {:docs_v1, anno, _, _, _, metadata, _} -> {anno_line(anno), nil, metadata}
    end
  end

  defp get_abstract_code(module) do
    {^module, binary, _file} = :code.get_object_code(module)

    case :beam_lib.chunks(binary, [:abstract_code]) do
      {:ok, {_, [{:abstract_code, {_vsn, abstract_code}}]}} -> abstract_code
      _otherwise -> []
    end
  end

  ## Function helpers

  defp get_docs(%{type: type, docs: docs} = module_data, source) do
    {:docs_v1, _, _, _, _, _, docs} = docs

    for doc <- docs, doc?(doc, type) do
      get_function(doc, source, module_data)
    end
  end

  # We are only interested in functions and macros for now
  defp doc?({{kind, _, _}, _, _, _, _}, _) when kind not in [:function, :macro] do
    false
  end

  # Skip impl_for and impl_for! for protocols
  defp doc?({{_, name, _}, _, _, :none, _}, :protocol) when name in [:impl_for, :impl_for!] do
    false
  end

  # Skip docs explicitly marked as hidden
  defp doc?({_, _, _, :hidden, _}, _) do
    false
  end

  # Skip default docs if starting with _
  defp doc?({{_, name, _}, _, _, :none, _}, _type) do
    hd(Atom.to_charlist(name)) != ?_
  end

  # Everything else is ok
  defp doc?(_, _) do
    true
  end

  defp get_function(function, source, module_data) do
    {{type, name, arity}, anno, signature, doc, metadata} = function
    actual_def = actual_def(name, arity, type)
    doc_line = anno_line(anno)
    annotations = annotations_from_metadata(metadata)

    line = find_function_line(module_data, actual_def) || doc_line
    doc = docstring(doc, name, arity, type, Map.fetch(module_data.impls, {name, arity}))
    defaults = get_defaults(name, arity, Map.get(metadata, :defaults, 0))

    specs =
      module_data.specs
      |> Map.get(actual_def, [])
      |> Enum.map(&Code.Typespec.spec_to_quoted(name, &1))

    specs =
      if type == :macro do
        Enum.map(specs, &remove_first_macro_arg/1)
      else
        specs
      end

    annotations =
      case {type, name, arity} do
        {:macro, _, _} -> ["macro" | annotations]
        {_, :__struct__, 0} -> ["struct" | annotations]
        _ -> annotations
      end

    %ExDoc.FunctionNode{
      id: "#{name}/#{arity}",
      name: name,
      arity: arity,
      deprecated: metadata[:deprecated],
      doc: doc,
      doc_line: doc_line,
      defaults: defaults,
      signature: Enum.join(signature, " "),
      specs: specs,
      source_path: source.path,
      source_url: source_link(source, line),
      type: if(metadata[:guard], do: :guard, else: type),
      annotations: annotations
    }
  end

  defp docstring(:none, name, arity, type, {:ok, behaviour}) do
    info = "Callback implementation for `c:#{inspect(behaviour)}.#{name}/#{arity}`."

    with {:docs_v1, _, _, _, _, _, docs} <- Code.fetch_docs(behaviour),
         key = {definition_to_callback(type), name, arity},
         {_, _, _, %{"en" => doc}, _} <- List.keyfind(docs, key, 0) do
      "#{doc}\n\n#{info}"
    else
      _ -> info
    end
  end

  defp docstring(doc, _, _, _, _), do: docstring(doc)

  defp definition_to_callback(:function), do: :callback
  defp definition_to_callback(:macro), do: :macrocallback

  defp get_defaults(_name, _arity, 0), do: []

  defp get_defaults(name, arity, defaults) do
    for default <- (arity - defaults)..(arity - 1), do: "#{name}/#{default}"
  end

  ## Callback helpers

  defp get_callbacks(%{type: :behaviour, name: name, abst_code: abst_code, docs: docs}, source) do
    {:docs_v1, _, _, _, _, _, docs} = docs
    optional_callbacks = name.behaviour_info(:optional_callbacks)

    for {{kind, _, _}, _, _, _, _} = doc <- docs, kind in [:callback, :macrocallback] do
      get_callback(doc, source, optional_callbacks, abst_code)
    end
  end

  defp get_callbacks(_, _), do: []

  defp get_callback(callback, source, optional_callbacks, abst_code) do
    {{kind, name, arity}, anno, _, doc, metadata} = callback
    actual_def = actual_def(name, arity, kind)
    doc_line = anno_line(anno)
    annotations = annotations_from_metadata(metadata)

    {:attribute, anno, :callback, {^actual_def, specs}} =
      Enum.find(abst_code, &match?({:attribute, _, :callback, {^actual_def, _}}, &1))

    line = anno_line(anno) || doc_line
    specs = Enum.map(specs, &Code.Typespec.spec_to_quoted(name, &1))

    annotations =
      if actual_def in optional_callbacks, do: ["optional" | annotations], else: annotations

    %ExDoc.FunctionNode{
      id: "#{name}/#{arity}",
      name: name,
      arity: arity,
      deprecated: metadata[:deprecated],
      doc: docstring(doc),
      doc_line: doc_line,
      signature: get_typespec_signature(hd(specs), arity),
      specs: specs,
      source_path: source.path,
      source_url: source_link(source, line),
      type: kind,
      annotations: annotations
    }
  end

  ## Typespecs

  # Returns a map of {name, arity} -> spec.
  defp get_specs(module) do
    case Code.Typespec.fetch_specs(module) do
      {:ok, specs} -> Map.new(specs)
      :error -> %{}
    end
  end

  # Returns a map of {name, arity} -> behaviour.
  defp get_impls(module) do
    for behaviour <- behaviours_implemented_by(module),
        callback <- callbacks_defined_by(behaviour),
        do: {callback, behaviour},
        into: %{}
  end

  defp callbacks_defined_by(module) do
    case Code.Typespec.fetch_callbacks(module) do
      {:ok, callbacks} -> Keyword.keys(callbacks)
      :error -> []
    end
  end

  defp behaviours_implemented_by(module) do
    for {:behaviour, list} <- module.module_info(:attributes),
        behaviour <- list,
        do: behaviour
  end

  defp get_types(%{docs: docs} = module_data, source) do
    {:docs_v1, _, _, _, _, _, docs} = docs

    for {{:type, _, _}, _, _, content, _} = doc <- docs, content != :hidden do
      get_type(doc, source, module_data.abst_code)
    end
  end

  defp get_type(type, source, abst_code) do
    {{_, name, arity}, anno, _, doc, metadata} = type
    doc_line = anno_line(anno)
    annotations = annotations_from_metadata(metadata)

    {:attribute, anno, type, spec} =
      Enum.find(abst_code, fn
        {:attribute, _, type, {^name, _, args}} ->
          type in [:opaque, :type] and length(args) == arity

        _ ->
          false
      end)

    spec = spec |> Code.Typespec.type_to_quoted() |> process_type_ast(type)
    line = anno_line(anno) || doc_line

    annotations = if type == :opaque, do: ["opaque" | annotations], else: annotations

    %ExDoc.TypeNode{
      id: "#{name}/#{arity}",
      name: name,
      arity: arity,
      type: type,
      spec: spec,
      deprecated: metadata[:deprecated],
      doc: docstring(doc),
      doc_line: doc_line,
      signature: get_typespec_signature(spec, arity),
      source_path: source.path,
      source_url: source_link(source, line),
      annotations: annotations
    }
  end

  # Cut off the body of an opaque type while leaving it on a normal type.
  defp process_type_ast({:::, _, [d | _]}, :opaque), do: d
  defp process_type_ast(ast, _), do: ast

  defp get_typespec_signature({:when, _, [{:::, _, [{name, meta, args}, _]}, _]}, arity) do
    Macro.to_string({name, meta, strip_types(args, arity)})
  end

  defp get_typespec_signature({:::, _, [{name, meta, args}, _]}, arity) do
    Macro.to_string({name, meta, strip_types(args, arity)})
  end

  defp get_typespec_signature({name, meta, args}, arity) do
    Macro.to_string({name, meta, strip_types(args, arity)})
  end

  defp strip_types(args, arity) do
    args
    |> Enum.take(-arity)
    |> Enum.with_index()
    |> Enum.map(fn
      {{:::, _, [left, _]}, i} -> to_var(left, i)
      {{:|, _, _}, i} -> to_var({}, i)
      {left, i} -> to_var(left, i)
    end)
  end

  defp to_var({name, meta, _}, _) when is_atom(name), do: {name, meta, nil}
  defp to_var([{:->, _, _} | _], _), do: {:function, [], nil}
  defp to_var({:<<>>, _, _}, _), do: {:binary, [], nil}
  defp to_var({:%{}, _, _}, _), do: {:map, [], nil}
  defp to_var({:{}, _, _}, _), do: {:tuple, [], nil}
  defp to_var({_, _}, _), do: {:tuple, [], nil}
  defp to_var(integer, _) when is_integer(integer), do: {:integer, [], nil}
  defp to_var(float, _) when is_integer(float), do: {:float, [], nil}
  defp to_var(list, _) when is_list(list), do: {:list, [], nil}
  defp to_var(atom, _) when is_atom(atom), do: {:atom, [], nil}
  defp to_var(_, i), do: {:"arg#{i}", [], nil}

  ## General helpers

  defp actual_def(name, arity, :macrocallback) do
    {String.to_atom("MACRO-" <> to_string(name)), arity + 1}
  end

  defp actual_def(name, arity, :macro) do
    {String.to_atom("MACRO-" <> to_string(name)), arity + 1}
  end

  defp actual_def(name, arity, _), do: {name, arity}

  defp annotations_from_metadata(metadata) do
    annotations = []

    annotations =
      if since = metadata[:since] do
        ["since #{since}" | annotations]
      else
        annotations
      end

    annotations
  end

  defp remove_first_macro_arg({:::, info, [{name, info2, [_term_arg | rest_args]}, return]}) do
    {:::, info, [{name, info2, rest_args}, return]}
  end

  defp find_module_line(%{abst_code: abst_code, name: name}) do
    Enum.find_value(abst_code, fn
      {:attribute, anno, :module, ^name} -> anno_line(anno)
      _ -> nil
    end)
  end

  defp find_function_line(%{abst_code: abst_code}, {name, arity}) do
    Enum.find_value(abst_code, fn
      {:function, anno, ^name, ^arity, _} -> anno_line(anno)
      _ -> nil
    end)
  end

  defp docstring(%{"en" => doc}), do: doc
  defp docstring(_), do: nil

  defp anno_line(line) when is_integer(line), do: abs(line)
  defp anno_line(anno), do: anno |> :erl_anno.line() |> abs()

  defp source_link(%{path: _, url: nil}, _line), do: nil

  defp source_link(source, line) do
    source_url = Regex.replace(~r/%{path}/, source.url, source.path)
    Regex.replace(~r/%{line}/, source_url, to_string(line))
  end

  defp source_path(module, config) do
    source = String.Chars.to_string(module.__info__(:compile)[:source])

    if root = config.source_root do
      Path.relative_to(source, root)
    else
      source
    end
  end

  defp module_title_and_id(%{name: module, type: :task}) do
    {task_name(module), module_id(module)}
  end

  defp module_title_and_id(%{name: module}) do
    id = module_id(module)
    {id, id}
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
end
