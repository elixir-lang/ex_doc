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
    modules
    |> Task.async_stream(&get_module(&1, config), timeout: :infinity)
    |> Enum.reduce(acc, fn {:ok, result}, {modules, filtered} = acc ->
      case result do
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
      {GroupMatcher.index(config.groups_for_modules, module.group), module.nested_context,
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
            ExDoc.Utils.warn("skipping docs for module #{inspect(module)}, reason: #{reason}", [])
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
    source = %{
      url_pattern: config.source_url_pattern,
      path: module_data.source_file,
      relative_path: path_relative_to_cwd(module_data.source_file)
    }

    {doc_line, doc_file, format, source_doc, doc_ast, metadata} =
      get_module_docs(module_data, source)

    group_for_doc = config.group_for_doc
    annotations_for_docs = config.annotations_for_docs

    docs = get_docs(module_data, source, group_for_doc, annotations_for_docs)
    metadata = Map.put(metadata, :kind, module_data.type)
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
      docs_groups: config.docs_groups ++ module_data.default_groups,
      docs: ExDoc.Utils.natural_sort_by(docs, &"#{&1.name}/#{&1.arity}"),
      doc: normalize_doc_ast(doc_ast, "module-"),
      source_doc: source_doc,
      source_format: format,
      moduledoc_line: doc_line,
      moduledoc_file: doc_file,
      source_url: source_link(source, module_data.source_line),
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

  defp normalize_doc_ast(doc_ast, prefix) do
    doc_ast
    |> DocAST.add_ids_to_headers([:h2, :h3], prefix)
  end

  # Helpers

  defp get_module_docs(module_data, source) do
    {:docs_v1, anno, _, format, moduledoc, metadata, _} = module_data.docs
    doc_file = anno_file(anno, source)
    doc_line = anno_line(anno)
    options = [file: doc_file, line: doc_line + 1]
    {doc_line, doc_file, format, moduledoc, doc_ast(format, moduledoc, options), metadata}
  end

  defp get_docs(module_data, source, group_for_doc, annotations_for_docs) do
    {:docs_v1, _, _, _, _, _, docs} = module_data.docs

    nodes =
      for doc <- docs,
          doc_data = module_data.language.doc_data(doc, module_data) do
        get_doc(doc, doc_data, module_data, source, group_for_doc, annotations_for_docs)
      end

    filter_defaults(nodes)
  end

  defp get_doc(doc, doc_data, module_data, source, group_for_doc, annotations_for_docs) do
    {:docs_v1, _, _, content_type, _, module_metadata, _} = module_data.docs
    {{type, name, arity}, anno, _signature, source_doc, metadata} = doc
    doc_file = anno_file(anno, source)
    doc_line = anno_line(anno)

    metadata =
      Map.merge(
        %{kind: type, name: name, arity: arity, module: module_data.module},
        metadata
      )

    source_url = source_link(doc_data.source_file, source, doc_data.source_line)

    annotations =
      annotations_for_docs.(metadata) ++
        annotations_from_metadata(metadata, module_metadata) ++ doc_data.extra_annotations

    defaults = get_defaults(name, arity, Map.get(metadata, :defaults, 0))

    doc_ast =
      doc_ast(content_type, source_doc, file: doc_file, line: doc_line + 1) ||
        doc_data.doc_fallback.()

    group = group_for_doc.(metadata) || doc_data.default_group
    id = doc_data.id_key <> nil_or_name(name, arity)

    %ExDoc.DocNode{
      id: id,
      name: name,
      arity: arity,
      deprecated: metadata[:deprecated],
      doc: normalize_doc_ast(doc_ast, id <> "-"),
      source_doc: source_doc,
      doc_line: doc_line,
      doc_file: doc_file,
      defaults: ExDoc.Utils.natural_sort_by(defaults, fn {name, arity} -> "#{name}/#{arity}" end),
      signature: signature(doc_data.signature),
      specs: doc_data.specs,
      source_url: source_url,
      type: doc_data.type,
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

  ## General helpers

  defp nil_or_name(name, arity) do
    if name == nil do
      "nil/#{arity}"
    else
      "#{name}/#{arity}"
    end
  end

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

  defp anno_file(anno, source) do
    case :erl_anno.file(anno) do
      :undefined ->
        source.relative_path

      file ->
        source.path
        |> Path.dirname()
        |> Path.join(file)
        |> path_relative_to_cwd()
    end
  end

  # TODO: Remove when we require Elixir 1.16
  if function_exported?(Path, :relative_to_cwd, 2) do
    defp path_relative_to_cwd(path), do: Path.relative_to_cwd(path, force: true)
  else
    defp path_relative_to_cwd(path), do: Path.relative_to_cwd(path)
  end

  defp source_link(nil, source, line), do: source_link(source, line)

  defp source_link(file, %{url_pattern: url_pattern}, line) do
    url_pattern.(path_relative_to_cwd(file), line)
  end

  defp source_link(%{url_pattern: url_pattern, relative_path: path}, line) do
    url_pattern.(path, line)
  end
end
