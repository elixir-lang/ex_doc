defmodule ExDoc.Retriever do
  # Functions to extract documentation information from modules.
  @moduledoc false

  defmodule Error do
    @moduledoc false
    defexception [:message]
  end

  alias ExDoc.{GroupMatcher, Refs, Markdown}
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
      {GroupMatcher.group_index(config.groups_for_modules, module.group), module.nested_context,
       module.nested_title, module.id}
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
    if docs_chunk = docs_chunk(module) do
      generate_node(module, docs_chunk, config)
    else
      []
    end
  end

  defp nesting_info(title, prefixes) do
    prefixes
    |> Enum.find(&String.starts_with?(title, &1 <> "."))
    |> case do
      nil -> {nil, nil}
      prefix -> {String.trim_leading(title, prefix <> "."), prefix}
    end
  end

  # Special case required for Elixir
  defp docs_chunk(:elixir_bootstrap), do: false
  defp docs_chunk(Elixir), do: false

  defp docs_chunk(module) do
    unless function_exported?(Code, :fetch_docs, 1) do
      raise Error,
            "ExDoc 0.19+ requires Elixir v1.7 and later. " <>
              "For earlier Elixir versions, make sure to depend on {:ex_doc, \"~> 0.18.0\"}"
    end

    result = ExDoc.Utils.Code.fetch_docs(module)
    Refs.insert_from_chunk(module, result)

    case result do
      # TODO: Once we require Elixir v1.12, we only keep modules that have map contents
      {:docs_v1, _, _, _, :hidden, _, _} ->
        false

      {:docs_v1, _, _, _, _, _, _} = docs ->
        _ = Code.ensure_loaded(module)
        docs

      {:error, reason} ->
        cond do
          not Code.ensure_loaded?(module) ->
            raise Error, "module #{inspect(module)} is not defined/available"

          not function_exported?(module, :__info__, 1) ->
            # Not an Elixir module, ignore until we have Erlang support
            false

          true ->
            raise Error,
                  "module #{inspect(module)} was not compiled with flag --docs: #{inspect(reason)}"
        end

      _ ->
        raise Error,
              "unknown format in Docs chunk. This likely means you are running on " <>
                "a more recent Elixir version that is not supported by ExDoc. Please update."
    end
  end

  defp generate_node(module, docs_chunk, config) do
    module_data = get_module_data(module, docs_chunk)

    case module_data do
      %{type: :impl} -> []
      _ -> [do_generate_node(module, module_data, config)]
    end
  end

  defp do_generate_node(module, module_data, config) do
    source_url = config.source_url_pattern
    source_path = source_path(module, config)
    source = %{url: source_url, path: source_path}

    {doc_line, moduledoc, metadata} = get_module_docs(module_data, source_path)
    line = find_module_line(module_data) || doc_line

    {function_groups, function_docs} = get_docs(module_data, source, config)
    docs = function_docs ++ get_callbacks(module_data, source)
    types = get_types(module_data, source)
    {title, id} = module_title_and_id(module_data)
    {nested_title, nested_context} = nesting_info(title, config.nest_modules_by_prefix)

    node = %ExDoc.ModuleNode{
      id: id,
      title: title,
      nested_title: nested_title,
      nested_context: nested_context,
      module: module,
      type: module_data.type,
      deprecated: metadata[:deprecated],
      function_groups: function_groups,
      docs: Enum.sort_by(docs, &sort_key(&1.name, &1.arity)),
      doc: moduledoc,
      doc_line: doc_line,
      typespecs: Enum.sort_by(types, &{&1.name, &1.arity}),
      source_path: source_path,
      source_url: source_link(source, line)
    }

    put_in(node.group, GroupMatcher.match_module(config.groups_for_modules, node))
  end

  defp sort_key(name, arity) do
    first = name |> Atom.to_charlist() |> hd()
    {first in ?a..?z, name, arity}
  end

  defp doc_ast("text/markdown", %{"en" => doc}, options),
    do: Markdown.to_ast(doc, options)

  defp doc_ast(other, %{"en" => _}, _),
    do: raise("content type #{inspect(other)} is not supported")

  defp doc_ast(_, _, _options),
    do: nil

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

  defp get_module_docs(module_data, source_path) do
    {:docs_v1, anno, _, content_type, moduledoc, metadata, _} = module_data.docs
    doc_line = anno_line(anno)
    options = [file: source_path, line: doc_line + 1]
    {doc_line, doc_ast(content_type, moduledoc, options), metadata}
  end

  defp get_abstract_code(module) do
    {^module, binary, _file} = :code.get_object_code(module)

    case :beam_lib.chunks(binary, [:abstract_code]) do
      {:ok, {_, [{:abstract_code, {_vsn, abstract_code}}]}} -> abstract_code
      _otherwise -> []
    end
  end

  ## Function helpers

  defp get_docs(%{type: type, docs: docs} = module_data, source, config) do
    {:docs_v1, _, _, _, _, _, docs} = docs

    groups_for_functions =
      Enum.map(config.groups_for_functions, fn {group, filter} ->
        {Atom.to_string(group), filter}
      end) ++ [{"Functions", fn _ -> true end}]

    function_docs =
      for doc <- docs, doc?(doc, type) do
        get_function(doc, source, module_data, groups_for_functions)
      end

    {Enum.map(groups_for_functions, &elem(&1, 0)), filter_defaults(function_docs)}
  end

  # We are only interested in functions and macros for now
  defp doc?({{kind, _, _}, _, _, _, _}, _) when kind not in [:function, :macro] do
    false
  end

  # Skip impl_for and impl_for! for protocols
  defp doc?({{_, name, _}, _, _, _, _}, :protocol) when name in [:impl_for, :impl_for!] do
    false
  end

  # If content is a map, then it is ok.
  defp doc?({_, _, _, %{}, _}, _) do
    true
  end

  # We keep this clause with backwards compatibility with Elixir,
  # from v1.12+, functions not starting with _ always default to %{}.
  # TODO: Remove me once we require Elixir v1.12.
  defp doc?({{_, name, _}, _, _, :none, _}, _type) do
    hd(Atom.to_charlist(name)) != ?_
  end

  # Everything else is hidden.
  defp doc?({_, _, _, _, _}, _) do
    false
  end

  defp get_function(function, source, module_data, groups_for_functions) do
    {:docs_v1, _, _, content_type, _, _, _} = module_data.docs
    {{type, name, arity}, anno, signature, doc, metadata} = function
    actual_def = actual_def(name, arity, type)
    doc_line = anno_line(anno)
    annotations = annotations_from_metadata(metadata)

    line = find_function_line(module_data, actual_def) || doc_line
    impl = Map.fetch(module_data.impls, actual_def)
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

    group =
      Enum.find_value(groups_for_functions, fn {group, filter} ->
        filter.(metadata) && group
      end)

    doc_ast =
      (doc && doc_ast(content_type, doc, file: source.path, line: doc_line + 1)) ||
        callback_doc_ast(name, arity, impl) ||
        delegate_doc_ast(metadata[:delegate_to])

    %ExDoc.FunctionNode{
      id: "#{name}/#{arity}",
      name: name,
      arity: arity,
      deprecated: metadata[:deprecated],
      doc: doc_ast,
      doc_line: doc_line,
      defaults: Enum.sort_by(defaults, fn {name, arity} -> sort_key(name, arity) end),
      signature: Enum.join(signature, " "),
      specs: specs,
      source_path: source.path,
      source_url: source_link(source, line),
      type: type,
      group: group,
      annotations: annotations
    }
  end

  defp delegate_doc_ast({m, f, a}),
    do: [{:p, [], ["See ", {:code, [class: "inline"], [Exception.format_mfa(m, f, a)]}, "."]}]

  defp delegate_doc_ast(nil),
    do: nil

  defp callback_doc_ast(name, arity, {:ok, behaviour}),
    do: [
      {:p, [],
       [
         "Callback implementation for ",
         {:code, [class: "inline"], ["c:#{inspect(behaviour)}.#{name}/#{arity}"]},
         "."
       ]}
    ]

  defp callback_doc_ast(_, _, _),
    do: nil

  defp get_defaults(_name, _arity, 0), do: []

  defp get_defaults(name, arity, defaults) do
    for default <- (arity - defaults)..(arity - 1), do: {name, default}
  end

  defp filter_defaults(docs) do
    Enum.map(docs, &filter_defaults(&1, docs))
  end

  defp filter_defaults(doc, docs) do
    update_in(doc.defaults, fn defaults ->
      Enum.reject(defaults, fn {name, arity} ->
        Enum.any?(docs, &match?(%{name: ^name, arity: ^arity}, &1))
      end)
    end)
  end

  ## Callback helpers

  defp get_callbacks(%{type: :behaviour} = module_data, source) do
    {:docs_v1, _, _, _, _, _, docs} = module_data.docs
    optional_callbacks = module_data.name.behaviour_info(:optional_callbacks)

    for {{kind, _, _}, _, _, _, _} = doc <- docs, kind in [:callback, :macrocallback] do
      get_callback(doc, source, optional_callbacks, module_data)
    end
  end

  defp get_callbacks(_, _), do: []

  defp get_callback(callback, source, optional_callbacks, module_data) do
    {:docs_v1, _, _, content_type, _, _, _} = module_data.docs
    {{kind, name, arity}, anno, _, doc, metadata} = callback
    actual_def = actual_def(name, arity, kind)
    doc_line = anno_line(anno)
    annotations = annotations_from_metadata(metadata)

    {:attribute, anno, :callback, {^actual_def, specs}} =
      Enum.find(module_data.abst_code, &match?({:attribute, _, :callback, {^actual_def, _}}, &1))

    line = anno_line(anno) || doc_line
    specs = Enum.map(specs, &Code.Typespec.spec_to_quoted(name, &1))

    annotations =
      if actual_def in optional_callbacks, do: ["optional" | annotations], else: annotations

    doc_ast = doc_ast(content_type, doc, file: source.path, line: doc_line + 1)

    %ExDoc.FunctionNode{
      id: "#{name}/#{arity}",
      name: name,
      arity: arity,
      deprecated: metadata[:deprecated],
      doc: doc_ast,
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

  # Returns a map of {name, arity} => spec.
  defp get_specs(module) do
    case Code.Typespec.fetch_specs(module) do
      {:ok, specs} -> Map.new(specs)
      :error -> %{}
    end
  end

  # Returns a map of {name, arity} => behaviour.
  defp get_impls(module) do
    for behaviour <- behaviours_implemented_by(module),
        callback <- callbacks_defined_by(behaviour),
        do: {callback, behaviour},
        into: %{}
  end

  defp callbacks_defined_by(module) do
    case Code.Typespec.fetch_callbacks(module) do
      {:ok, callbacks} -> Enum.map(callbacks, &elem(&1, 0))
      :error -> []
    end
  end

  defp behaviours_implemented_by(module) do
    for {:behaviour, list} <- module.module_info(:attributes),
        behaviour <- list,
        do: behaviour
  end

  defp get_types(module_data, source) do
    {:docs_v1, _, _, _, _, _, docs} = module_data.docs

    # TODO: When we require Elixir v1.12, we only keep contents that are maps
    for {{:type, _, _}, _, _, content, _} = doc <- docs, content != :hidden do
      get_type(doc, source, module_data)
    end
  end

  defp get_type(type, source, module_data) do
    {:docs_v1, _, _, content_type, _, _, _} = module_data.docs
    {{_, name, arity}, anno, _, doc, metadata} = type
    doc_line = anno_line(anno)
    annotations = annotations_from_metadata(metadata)

    {:attribute, anno, type, spec} =
      Enum.find(module_data.abst_code, fn
        {:attribute, _, type, {^name, _, args}} ->
          type in [:opaque, :type] and length(args) == arity

        _ ->
          false
      end)

    spec = spec |> Code.Typespec.type_to_quoted() |> process_type_ast(type)
    line = anno_line(anno) || doc_line

    annotations = if type == :opaque, do: ["opaque" | annotations], else: annotations
    doc_ast = doc_ast(content_type, doc, file: source.path)

    %ExDoc.TypeNode{
      id: "#{name}/#{arity}",
      name: name,
      arity: arity,
      type: type,
      spec: spec,
      deprecated: metadata[:deprecated],
      doc: doc_ast,
      doc_line: doc_line,
      signature: get_typespec_signature(spec, arity),
      source_path: source.path,
      source_url: source_link(source, line),
      annotations: annotations
    }
  end

  # Cut off the body of an opaque type while leaving it on a normal type.
  defp process_type_ast({:"::", _, [d | _]}, :opaque), do: d
  defp process_type_ast(ast, _), do: ast

  defp get_typespec_signature({:when, _, [{:"::", _, [{name, meta, args}, _]}, _]}, arity) do
    Macro.to_string({name, meta, strip_types(args, arity)})
  end

  defp get_typespec_signature({:"::", _, [{name, meta, args}, _]}, arity) do
    Macro.to_string({name, meta, strip_types(args, arity)})
  end

  defp get_typespec_signature({name, meta, args}, arity) do
    Macro.to_string({name, meta, strip_types(args, arity)})
  end

  defp strip_types(args, arity) do
    args
    |> Enum.take(-arity)
    |> Enum.with_index(1)
    |> Enum.map(fn
      {{:"::", _, [left, _]}, position} -> to_var(left, position)
      {{:|, _, _}, position} -> to_var({}, position)
      {left, position} -> to_var(left, position)
    end)
  end

  defp to_var({:%, meta, [name, _]}, _), do: {:%, meta, [name, {:%{}, meta, []}]}
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
  defp to_var(_, position), do: {:"arg#{position}", [], nil}

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

  defp remove_first_macro_arg({:"::", info, [{name, info2, [_term_arg | rest_args]}, return]}) do
    {:"::", info, [{name, info2, rest_args}, return]}
  end

  defp remove_first_macro_arg({:when, meta, [lhs, rhs]}) do
    {:when, meta, [remove_first_macro_arg(lhs), rhs]}
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
    {"mix " <> task_name(module), module_id(module)}
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
