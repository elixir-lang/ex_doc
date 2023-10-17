defmodule ExDoc.Retriever do
  # Functions to extract documentation information from modules.
  @moduledoc false

  defmodule Error do
    @moduledoc false
    defexception [:message]
  end

  alias ExDoc.{DocAST, GroupMatcher, Refs, Utils}
  alias ExDoc.Retriever.Error

  @doc """
  Extract documentation from all modules in the specified directory or directories.

  Returns a tuple containing `{modules, filtered}`, using `config.filter_modules`
  as a filter criteria.
  """
  @spec docs_from_dir(Path.t() | [Path.t()], ExDoc.Config.t()) ::
          {[ExDoc.ModuleNode.t()], [ExDoc.ModuleNode.t()]}
  def docs_from_dir(dir, config) when is_binary(dir) do
    dir
    |> docs_from_dir({[], []}, config)
    |> sort_modules(config)
  end

  def docs_from_dir(dirs, config) when is_list(dirs) do
    dirs
    |> Enum.reduce({[], []}, &docs_from_dir(&1, &2, config))
    |> sort_modules(config)
  end

  defp docs_from_dir(dir, acc, config) do
    files = Path.wildcard(Path.expand("*.beam", dir))

    files
    |> Enum.map(&filename_to_module/1)
    |> docs_from_modules(acc, config)
  end

  @doc """
  Extract documentation from all modules and returns a tuple containing
  `{modules, filtered}`, two lists of modules that were extracted and filtered
  by `config.filter_modules`, respectively.
  """
  @spec docs_from_modules([atom], ExDoc.Config.t()) ::
          {[ExDoc.ModuleNode.t()], [ExDoc.ModuleNode.t()]}
  def docs_from_modules(modules, config) when is_list(modules) do
    modules |> docs_from_modules({[], []}, config) |> sort_modules(config)
  end

  defp docs_from_modules(modules, acc, config) do
    Enum.reduce(modules, acc, fn module_name, {modules, filtered} = acc ->
      case get_module(module_name, config) do
        {:error, _module} ->
          acc

        {:ok, module_node} ->
          if config.filter_modules.(module_node.module, module_node.metadata),
            do: {[module_node | modules], filtered},
            else: {modules, [module_node | filtered]}
      end
    end)
  end

  defp sort_modules({modules, filtered}, config) do
    {sort_modules(modules, config), sort_modules(filtered, config)}
  end

  defp sort_modules(modules, config) when is_list(modules) do
    Enum.sort_by(modules, fn module ->
      {GroupMatcher.group_index(config.groups_for_modules, module.group), module.nested_context,
       module.nested_title, module.id}
    end)
  end

  defp filename_to_module(name) do
    name = Path.basename(name, ".beam")
    String.to_atom(name)
  end

  defp get_module(module, config) do
    with {:docs_v1, _, language, _, _, _metadata, _} = docs_chunk <- docs_chunk(module),
         {:ok, language} <- ExDoc.Language.get(language, module),
         %{} = module_data <- language.module_data(module, docs_chunk, config) do
      {:ok, generate_node(module, module_data, config)}
    else
      _ ->
        {:error, module}
    end
  end

  defp docs_chunk(module) do
    result = Code.fetch_docs(module)
    Refs.insert_from_chunk(module, result)

    case result do
      {:docs_v1, _, _, _, :hidden, _, _} ->
        false

      {:docs_v1, _, _, _, _, _, _} = docs ->
        case Code.ensure_loaded(module) do
          {:module, _} ->
            docs

          {:error, reason} ->
            IO.warn("skipping docs for module #{inspect(module)}, reason: #{reason}", [])
            false
        end

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
    {doc_line, format, source_doc, doc, metadata} = get_module_docs(module_data, source_path)

    # TODO: The default function groups must be returned by the language
    groups_for_docs =
      config.groups_for_docs ++
        [Callbacks: &(&1[:__doc__] == :callback), Functions: fn _ -> true end]

    annotations_for_docs = config.annotations_for_docs

    docs_groups = Enum.map(groups_for_docs, &elem(&1, 0))
    function_docs = get_docs(module_data, source, groups_for_docs, annotations_for_docs)

    docs =
      function_docs ++
        get_callbacks(module_data, source, groups_for_docs, annotations_for_docs)

    types = get_types(module_data, source)

    metadata = Map.put(metadata, :__doc__, module_data.type)
    group = GroupMatcher.match_module(config.groups_for_modules, module, module_data.id, metadata)
    {nested_title, nested_context} = module_data.nesting_info || {nil, nil}

    %ExDoc.ModuleNode{
      id: module_data.id,
      title: module_data.title,
      nested_title: nested_title,
      nested_context: nested_context,
      group: group,
      module: module,
      type: module_data.type,
      deprecated: metadata[:deprecated],
      docs_groups: docs_groups,
      docs: ExDoc.Utils.natural_sort_by(docs, &"#{&1.name}/#{&1.arity}"),
      doc_format: format,
      doc: doc,
      source_doc: source_doc,
      doc_line: doc_line,
      typespecs: ExDoc.Utils.natural_sort_by(types, &"#{&1.name}/#{&1.arity}"),
      source_path: source_path,
      source_url: source_link(source, module_data.line),
      language: module_data.language,
      annotations: List.wrap(metadata[:tags]),
      metadata: metadata
    }
  end

  defp doc_ast(format, %{"en" => doc_content}, options) do
    DocAST.parse!(doc_content, format, options)
  end

  defp doc_ast(_format, _, _options) do
    nil
  end

  # Module Helpers

  defp get_module_docs(module_data, source_path) do
    {:docs_v1, anno, _, format, moduledoc, metadata, _} = module_data.docs
    doc_line = anno_line(anno)
    options = [file: source_path, line: doc_line + 1]
    {doc_line, format, moduledoc, doc_ast(format, moduledoc, options), metadata}
  end

  ## Function helpers

  defp get_docs(module_data, source, groups_for_docs, annotations_for_docs) do
    {:docs_v1, _, _, _, _, _, doc_elements} = module_data.docs

    nodes =
      Enum.flat_map(doc_elements, fn doc_element ->
        case module_data.language.function_data(doc_element, module_data) do
          :skip ->
            []

          function_data ->
            [
              get_function(
                doc_element,
                function_data,
                source,
                module_data,
                groups_for_docs,
                annotations_for_docs
              )
            ]
        end
      end)

    filter_defaults(nodes)
  end

  defp get_function(
         doc_element,
         function_data,
         source,
         module_data,
         groups_for_docs,
         annotations_for_docs
       ) do
    {:docs_v1, _, _, content_type, _, module_metadata, _} = module_data.docs
    {{type, name, arity}, anno, signature, doc_content, metadata} = doc_element
    doc_line = anno_line(anno)

    annotations =
      annotations_for_docs.(metadata) ++
        annotations_from_metadata(metadata, module_metadata) ++ function_data.extra_annotations

    line = function_data.line || doc_line
    defaults = get_defaults(name, arity, Map.get(metadata, :defaults, 0))

    doc_ast =
      (doc_content && doc_ast(content_type, doc_content, file: source.path, line: doc_line + 1)) ||
        function_data.doc_fallback.()

    group = GroupMatcher.match_function(groups_for_docs, metadata)

    %ExDoc.FunctionNode{
      id: "#{name}/#{arity}",
      name: name,
      arity: arity,
      deprecated: metadata[:deprecated],
      doc: doc_ast,
      source_doc: doc_content,
      doc_line: doc_line,
      defaults: ExDoc.Utils.natural_sort_by(defaults, fn {name, arity} -> "#{name}/#{arity}" end),
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

  defp filter_defaults(nodes) do
    Enum.map(nodes, &filter_defaults(&1, nodes))
  end

  defp filter_defaults(node, nodes) do
    update_in(node.defaults, fn defaults ->
      Enum.reject(defaults, fn {name, arity} ->
        Enum.any?(nodes, &match?(%{name: ^name, arity: ^arity}, &1))
      end)
    end)
  end

  ## Callback helpers

  defp get_callbacks(
         %{type: :behaviour} = module_data,
         source,
         groups_for_docs,
         annotations_for_docs
       ) do
    {:docs_v1, _, _, _, _, _, docs} = module_data.docs

    for {{kind, _, _}, _, _, _, _} = doc <- docs, kind in module_data.callback_types do
      get_callback(doc, source, groups_for_docs, module_data, annotations_for_docs)
    end
  end

  defp get_callbacks(_, _, _, _), do: []

  defp get_callback(callback, source, groups_for_docs, module_data, annotations_for_docs) do
    callback_data = module_data.language.callback_data(callback, module_data)

    {:docs_v1, _, _, content_type, _, module_metadata, _} = module_data.docs
    {{kind, name, arity}, anno, _signature, doc, metadata} = callback
    doc_line = anno_line(anno)

    signature = signature(callback_data.signature)
    specs = callback_data.specs

    annotations =
      annotations_for_docs.(metadata) ++
        callback_data.extra_annotations ++ annotations_from_metadata(metadata, module_metadata)

    doc_ast = doc_ast(content_type, doc, file: source.path, line: doc_line + 1)

    metadata = Map.put(metadata, :__doc__, :callback)
    group = GroupMatcher.match_function(groups_for_docs, metadata)

    %ExDoc.FunctionNode{
      id: "c:#{name}/#{arity}",
      name: name,
      arity: arity,
      deprecated: metadata[:deprecated],
      doc: doc_ast,
      source_doc: doc,
      doc_line: doc_line,
      signature: signature,
      specs: specs,
      source_path: source.path,
      source_url: source_link(source, callback_data.line),
      type: kind,
      annotations: annotations,
      group: group
    }
  end

  ## Typespecs

  defp get_types(module_data, source) do
    {:docs_v1, _, _, _, _, _, docs} = module_data.docs

    for {{:type, _, _}, _, _, content, _} = doc <- docs, content != :hidden do
      get_type(doc, source, module_data)
    end
  end

  defp get_type(type_entry, source, module_data) do
    {:docs_v1, _, _, content_type, _, module_metadata, _} = module_data.docs
    {{_, name, arity}, anno, _signature, doc, metadata} = type_entry
    doc_line = anno_line(anno)
    annotations = annotations_from_metadata(metadata, module_metadata)

    type_data = module_data.language.type_data(type_entry, module_data)
    signature = signature(type_data.signature)
    annotations = if type_data.type == :opaque, do: ["opaque" | annotations], else: annotations
    doc_ast = doc_ast(content_type, doc, file: source.path)

    %ExDoc.TypeNode{
      id: "t:#{name}/#{arity}",
      name: name,
      arity: arity,
      type: type_data.type,
      spec: type_data.spec,
      deprecated: metadata[:deprecated],
      doc: doc_ast,
      source_doc: doc,
      doc_line: doc_line,
      signature: signature,
      source_path: source.path,
      source_url: source_link(source, type_data.line),
      annotations: annotations
    }
  end

  ## General helpers

  defp signature(list) when is_list(list), do: Enum.join(list, " ")

  defp annotations_from_metadata(metadata, module_metadata) do
    # Give precedence to the function/callback/type metadata over the module metadata.
    cond do
      since = metadata[:since] -> ["since #{since}"]
      since = module_metadata[:since] -> ["since #{since}"]
      true -> []
    end
  end

  defp anno_line(line) when is_integer(line), do: abs(line)
  defp anno_line(anno), do: anno |> :erl_anno.line() |> abs()

  defp source_link(%{path: _, url: nil}, _line), do: nil

  defp source_link(source, line) do
    Utils.source_url_pattern(source.url, source.path, line)
  end

  defp source_path(module, _config) do
    module.module_info(:compile)[:source]
    |> String.Chars.to_string()
    |> Path.relative_to(File.cwd!())
  end
end
