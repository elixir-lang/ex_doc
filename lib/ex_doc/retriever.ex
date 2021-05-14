defmodule ExDoc.Retriever do
  # Functions to extract documentation information from modules.
  @moduledoc false

  defmodule Error do
    @moduledoc false
    defexception [:message]
  end

  alias ExDoc.{DocAST, GroupMatcher, Refs}
  alias ExDoc.Retriever.Error

  @doc """
  Extract documentation from all modules in the specified directory or directories.
  """
  @spec docs_from_dir(Path.t() | [Path.t()], ExDoc.Config.t()) :: [ExDoc.ModuleNode.t()]
  def docs_from_dir(dir, config) when is_binary(dir) do
    files = Path.wildcard(Path.expand("*.beam", dir))
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
      module_data =
        module
        |> get_module_data(docs_chunk)
        |> maybe_skip(config.filter_prefix)

      if not module_data.skip do
        [generate_node(module, module_data, config)]
      else
        []
      end
    else
      []
    end
  end

  defp maybe_skip(module_data, filter_prefix) do
    if filter_prefix && not String.starts_with?(module_data.id, filter_prefix) do
      %{module_data | skip: true}
    else
      module_data
    end
  end

  defp docs_chunk(module) do
    result = ExDoc.Utils.Code.fetch_docs(module)
    Refs.insert_from_chunk(module, result)

    case result do
      # TODO: Once we require Elixir v1.12, we only keep modules that have map contents
      {:docs_v1, _, _, _, :hidden, _, _} ->
        false

      {:docs_v1, _, _, _, _, _, _} = docs ->
        _ = Code.ensure_loaded(module)
        docs

      {:error, :chunk_not_found} ->
        false

      {:error, :module_not_found} ->
        unless Code.ensure_loaded?(module) do
          raise Error, "module #{inspect(module)} is not defined/available"
        end

      {:error, _} = error ->
        raise Error, "error accessing #{inspect(module)}: #{inspect(error)}"

      _ ->
        raise Error,
              "unknown format in Docs chunk. This likely means you are running on " <>
                "a more recent Elixir version that is not supported by ExDoc. Please update."
    end
  end

  defp generate_node(module, module_data, config) do
    source_url = config.source_url_pattern
    source_path = source_path(module, config)
    source = %{url: source_url, path: source_path}

    {doc_line, moduledoc, metadata} = get_module_docs(module_data, source_path)
    line = find_module_line(module_data) || doc_line

    {function_groups, function_docs} = get_docs(module_data, source, config)
    docs = function_docs ++ get_callbacks(module_data, source)
    types = get_types(module_data, source)

    {nested_title, nested_context} =
      nesting_info(module_data.title, config.nest_modules_by_prefix)

    node = %ExDoc.ModuleNode{
      id: module_data.id,
      title: module_data.title,
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
      source_url: source_link(source, line),
      proglang: module_data.language
    }

    put_in(node.group, GroupMatcher.match_module(config.groups_for_modules, node))
  end

  defp nesting_info(title, prefixes) do
    prefixes
    |> Enum.find(&String.starts_with?(title, &1 <> "."))
    |> case do
      nil -> {nil, nil}
      prefix -> {String.trim_leading(title, prefix <> "."), prefix}
    end
  end

  defp sort_key(name, arity) do
    first = name |> Atom.to_charlist() |> hd()
    {first in ?a..?z, name, arity}
  end

  defp doc_ast(format, %{"en" => doc_content}, options) do
    DocAST.parse!(doc_content, format, options)
  end

  defp doc_ast(_, _, _options) do
    nil
  end

  # Module Helpers

  defp get_module_data(module, docs_chunk) do
    {:docs_v1, _, language, _, _, _, _} = docs_chunk
    language = ExDoc.Language.get(language)
    extra = language.module_data(module)

    %{
      id: extra.id,
      title: extra.title,
      name: module,
      language: language,
      type: extra.type,
      skip: extra.skip,
      specs: get_specs(module),
      impls: get_impls(module),
      callbacks: get_callbacks(module),
      abst_code: get_abstract_code(module),
      docs: docs_chunk,
      callback_types: [:callback] ++ extra.extra_callback_types
    }
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
    {:docs_v1, _, _, _, _, _, doc_elements} = docs

    groups_for_functions =
      Enum.map(config.groups_for_functions, fn {group, filter} ->
        {Atom.to_string(group), filter}
      end) ++ [{"Functions", fn _ -> true end}]

    function_doc_elements =
      for doc_element <- doc_elements, doc?(doc_element, type) do
        get_function(doc_element, source, module_data, groups_for_functions)
      end

    {Enum.map(groups_for_functions, &elem(&1, 0)), filter_defaults(function_doc_elements)}
  end

  # TODO: Elixir specific
  # We are only interested in functions and macros for now
  defp doc?({{kind, _, _}, _, _, _, _}, _) when kind not in [:function, :macro] do
    false
  end

  # TODO: Elixir specific
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

  defp get_function(doc_element, source, module_data, groups_for_functions) do
    function_data = module_data.language.function_data(doc_element, module_data)

    {:docs_v1, _, _, content_type, _, _, _} = module_data.docs
    {{type, name, arity}, anno, signature, doc_content, metadata} = doc_element
    doc_line = anno_line(anno)
    annotations = annotations_from_metadata(metadata) ++ function_data.extra_annotations
    line = function_data.line_override || doc_line
    defaults = get_defaults(name, arity, Map.get(metadata, :defaults, 0))

    group =
      Enum.find_value(groups_for_functions, fn {group, filter} ->
        # TODO: should we call filter with the whole %FunctionNode{}, not just metadata?
        #       also, should we save off metadata on the node?
        filter.(metadata) && group
      end)

    doc_ast =
      (doc_content && doc_ast(content_type, doc_content, file: source.path, line: doc_line + 1)) ||
        (function_data.doc_fallback && function_data.doc_fallback.())

    %ExDoc.FunctionNode{
      id: "#{name}/#{arity}",
      name: name,
      arity: arity,
      deprecated: metadata[:deprecated],
      doc: doc_ast,
      doc_line: doc_line,
      defaults: Enum.sort_by(defaults, fn {name, arity} -> sort_key(name, arity) end),
      signature: signature(signature),
      specs: function_data.specs,
      source_path: source.path,
      source_url: source_link(source, line),
      type: type,
      group: group,
      annotations: annotations
    }
  end

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

    for {{kind, _, _}, _, _, _, _} = doc <- docs, kind in module_data.callback_types do
      get_callback(doc, source, optional_callbacks, module_data)
    end
  end

  defp get_callbacks(_, _), do: []

  defp get_callback(callback, source, optional_callbacks, module_data) do
    callback_data = module_data.language.callback_data(callback, module_data)

    {:docs_v1, _, _, content_type, _, _, _} = module_data.docs
    {{kind, name, arity}, anno, signature, doc, metadata} = callback
    actual_def = callback_data.actual_def
    doc_line = anno_line(anno)
    signature = signature(signature)
    specs = callback_data.specs

    signature =
      signature ||
        (callback_data.signature_fallback && callback_data.signature_fallback.()) ||
        "#{name}/#{arity}"

    annotations = annotations_from_metadata(metadata)

    # TODO: actual_def is Elixir specific, but remember optional_callbacks are generic.
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
      signature: signature,
      specs: specs,
      source_path: source.path,
      source_url: source_link(source, callback_data.line || doc_line),
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
        {callback, _} <- get_callbacks(behaviour),
        do: {callback, behaviour},
        into: %{}
  end

  defp get_callbacks(module) do
    case Code.Typespec.fetch_callbacks(module) do
      {:ok, callbacks} -> Map.new(callbacks)
      :error -> %{}
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

  defp get_type(type_entry, source, module_data) do
    {:docs_v1, _, _, content_type, _, _, _} = module_data.docs
    {{_, name, arity}, anno, signature, doc, metadata} = type_entry
    doc_line = anno_line(anno)
    annotations = annotations_from_metadata(metadata)

    {:attribute, anno, type, spec} =
      Enum.find(module_data.abst_code, fn
        {:attribute, _, type, {^name, _, args}} ->
          type in [:opaque, :type] and length(args) == arity

        _ ->
          false
      end)

    line = anno_line(anno)
    type_data = module_data.language.type_data(type_entry, spec, module_data)
    spec = type_data.spec

    signature =
      signature(signature) || (type_data.signature_fallback && type_data.signature_fallback.())

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
      signature: signature,
      source_path: source.path,
      source_url: source_link(source, line),
      annotations: annotations
    }
  end

  ## General helpers

  defp signature([]), do: nil
  defp signature(list) when is_list(list), do: Enum.join(list, " ")

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

  defp find_module_line(%{abst_code: abst_code, name: name}) do
    Enum.find_value(abst_code, fn
      {:attribute, anno, :module, ^name} -> anno_line(anno)
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
    source = String.Chars.to_string(module.module_info(:compile)[:source])

    if root = config.source_root do
      Path.relative_to(source, root)
    else
      source
    end
  end
end
