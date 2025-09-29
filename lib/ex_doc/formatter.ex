defmodule ExDoc.Formatter do
  @moduledoc false

  alias ExDoc.{Markdown, GroupMatcher, Utils}

  @doc """
  Autolinks and renders all docs.
  """
  def render_all(project_nodes, filtered_modules, ext, config, opts) do
    base = [
      apps: config.apps,
      deps: config.deps,
      ext: ext,
      extras: extra_paths(config),
      skip_undefined_reference_warnings_on: config.skip_undefined_reference_warnings_on,
      skip_code_autolink_to: config.skip_code_autolink_to,
      filtered_modules: filtered_modules
    ]

    project_nodes
    |> Task.async_stream(
      fn node ->
        language = node.language

        autolink_opts =
          [
            current_module: node.module,
            file: node.moduledoc_file,
            line: node.moduledoc_line,
            module_id: node.id,
            language: language
          ] ++ base

        docs =
          for child_node <- node.docs do
            id = id(node, child_node)

            autolink_opts =
              autolink_opts ++
                [
                  id: id,
                  line: child_node.doc_line,
                  file: child_node.doc_file,
                  current_kfa: {child_node.type, child_node.name, child_node.arity}
                ]

            specs = Enum.map(child_node.specs, &language.autolink_spec(&1, autolink_opts))
            child_node = %{child_node | specs: specs}
            render_doc(child_node, ext, language, autolink_opts, opts)
          end

        %{
          render_doc(node, ext, language, [{:id, node.id} | autolink_opts], opts)
          | docs: docs
        }
      end,
      timeout: :infinity
    )
    |> Enum.map(&elem(&1, 1))
  end

  defp render_doc(%{doc: nil} = node, _ext, _language, _autolink_opts, _opts),
    do: node

  defp render_doc(%{doc: doc} = node, ext, language, autolink_opts, opts) do
    rendered = autolink_and_render(doc, ext, language, autolink_opts, opts)
    %{node | rendered_doc: rendered}
  end

  defp id(%{id: mod_id}, %{id: "c:" <> id}) do
    "c:" <> mod_id <> "." <> id
  end

  defp id(%{id: mod_id}, %{id: "t:" <> id}) do
    "t:" <> mod_id <> "." <> id
  end

  defp id(%{id: mod_id}, %{id: id}) do
    mod_id <> "." <> id
  end

  defp autolink_and_render(doc, ".md", language, autolink_opts, _opts) do
    doc
    |> language.autolink_doc(autolink_opts)
    |> ExDoc.DocAST.to_markdown_string()
  end

  defp autolink_and_render(doc, _html_ext, language, autolink_opts, opts) do
    doc
    |> language.autolink_doc(autolink_opts)
    |> ExDoc.DocAST.to_string()
    |> ExDoc.DocAST.highlight(language, opts)
  end

  @doc """
  Builds extra nodes by normalizing the config entries.
  """
  def build_extras(config, ext) do
    groups = config.groups_for_extras

    language =
      case config.proglang do
        :erlang -> ExDoc.Language.Erlang
        _ -> ExDoc.Language.Elixir
      end

    source_url_pattern = config.source_url_pattern

    autolink_opts = [
      apps: config.apps,
      deps: config.deps,
      ext: ext,
      extras: extra_paths(config),
      language: language,
      skip_undefined_reference_warnings_on: config.skip_undefined_reference_warnings_on,
      skip_code_autolink_to: config.skip_code_autolink_to
    ]

    extras =
      config.extras
      |> Task.async_stream(
        &build_extra(&1, groups, ext, language, autolink_opts, source_url_pattern),
        timeout: :infinity
      )
      |> Enum.map(&elem(&1, 1))

    ids_count = Enum.reduce(extras, %{}, &Map.update(&2, &1.id, 1, fn c -> c + 1 end))

    extras
    |> Enum.map_reduce(1, fn extra, idx ->
      if ids_count[extra.id] > 1, do: {disambiguate_id(extra, idx), idx + 1}, else: {extra, idx}
    end)
    |> elem(0)
    |> Enum.sort_by(fn extra -> GroupMatcher.index(groups, extra.group) end)
  end

  @doc """
  Builds extra nodes in a format-agnostic way, preparing content for all formats.

  This function builds ExtraNode structures that contain the processed content
  for multiple formats, eliminating the need for each formatter to rebuild the same content.
  """
  def build_extras_for_extra_node(config) do
    groups = config.groups_for_extras

    language =
      case config.proglang do
        :erlang -> ExDoc.Language.Erlang
        _ -> ExDoc.Language.Elixir
      end

    source_url_pattern = config.source_url_pattern

    # Build base autolink options (without ext since we'll render for multiple formats)
    base_autolink_opts = [
      apps: config.apps,
      deps: config.deps,
      extras: extra_paths(config),
      language: language,
      skip_undefined_reference_warnings_on: config.skip_undefined_reference_warnings_on,
      skip_code_autolink_to: config.skip_code_autolink_to
    ]

    extras =
      config.extras
      |> Task.async_stream(
        &build_extra_node(&1, groups, language, base_autolink_opts, source_url_pattern),
        timeout: :infinity
      )
      |> Enum.map(&elem(&1, 1))

    ids_count = Enum.reduce(extras, %{}, &Map.update(&2, &1.id, 1, fn c -> c + 1 end))

    extras
    |> Enum.map_reduce(1, fn extra, idx ->
      if ids_count[extra.id] > 1, do: {disambiguate_extra_node_id(extra, idx), idx + 1}, else: {extra, idx}
    end)
    |> elem(0)
    |> Enum.sort_by(fn extra -> GroupMatcher.index(groups, extra.group) end)
  end

  defp build_extra_node(
         {input, input_options},
         groups,
         language,
         base_autolink_opts,
         source_url_pattern
       ) do
    input = to_string(input)
    id = input_options[:filename] || input |> filename_to_title() |> Utils.text_to_id()
    source_file = input_options[:source] || input
    opts = [file: source_file, line: 1]

    {source, ast} =
      case extension_name(input) do
        extension when extension in ["", ".txt"] ->
          source = File.read!(input)
          ast = [{:pre, [], "\n" <> source, %{}}]
          {source, ast}

        extension when extension in [".md", ".livemd", ".cheatmd"] ->
          source = File.read!(input)

          ast =
            source
            |> Markdown.to_ast(opts)
            |> sectionize(extension)

          {source, ast}

        _ ->
          raise ArgumentError,
                "file extension not recognized, allowed extension is either .cheatmd, .livemd, .md, .txt or no extension"
      end

    {title_ast, ast} =
      case ExDoc.DocAST.extract_title(ast) do
        {:ok, title_ast, ast} -> {title_ast, ast}
        :error -> {nil, ast}
      end

    title_text = title_ast && ExDoc.DocAST.text_from_ast(title_ast)
    title_html = title_ast && ExDoc.DocAST.to_string(title_ast)

    # Build content for all formats
    content = %{
      ast: ast,
      html: autolink_and_render(ast, ".html", language, [file: input, ext: ".html"] ++ base_autolink_opts, opts),
      epub: autolink_and_render(ast, ".xhtml", language, [file: input, ext: ".xhtml"] ++ base_autolink_opts, opts),
      # For markdown, use the original source to preserve markdown syntax without HTML attributes
      markdown: source
    }

    group = GroupMatcher.match_extra(groups, input)
    title = input_options[:title] || title_text || filename_to_title(input)

    source_path = source_file |> Path.relative_to(File.cwd!()) |> String.replace_leading("./", "")
    source_url = Utils.source_url_pattern(source_url_pattern, source_path, 1)

    %ExDoc.ExtraNode{
      id: id,
      title: title,
      title_content: title_html || title,
      source: source,
      source_path: source_path,
      source_url: source_url,
      group: group,
      content: content
    }
  end

  defp build_extra_node(input, groups, language, base_autolink_opts, source_url_pattern) do
    build_extra_node({input, []}, groups, language, base_autolink_opts, source_url_pattern)
  end

  defp disambiguate_extra_node_id(%ExDoc.ExtraNode{} = extra, idx) do
    %{extra | id: "#{extra.id}-#{idx}"}
  end

  defp build_extra(
         {input, input_options},
         groups,
         ext,
         language,
         autolink_opts,
         source_url_pattern
       ) do
    input = to_string(input)
    id = input_options[:filename] || input |> filename_to_title() |> Utils.text_to_id()
    source_file = input_options[:source] || input
    opts = [file: source_file, line: 1]

    {source, ast} =
      case extension_name(input) do
        extension when extension in ["", ".txt"] ->
          source = File.read!(input)
          ast = [{:pre, [], "\n" <> source, %{}}]
          {source, ast}

        extension when extension in [".md", ".livemd", ".cheatmd"] ->
          source = File.read!(input)

          ast =
            source
            |> Markdown.to_ast(opts)
            |> sectionize(extension)

          {source, ast}

        _ ->
          raise ArgumentError,
                "file extension not recognized, allowed extension is either .cheatmd, .livemd, .md, .txt or no extension"
      end

    {title_ast, ast} =
      case ExDoc.DocAST.extract_title(ast) do
        {:ok, title_ast, ast} -> {title_ast, ast}
        :error -> {nil, ast}
      end

    title_text = title_ast && ExDoc.DocAST.text_from_ast(title_ast)
    title_html = title_ast && ExDoc.DocAST.to_string(title_ast)
    content_html = autolink_and_render(ast, ext, language, [file: input] ++ autolink_opts, opts)

    group = GroupMatcher.match_extra(groups, input)
    title = input_options[:title] || title_text || filename_to_title(input)

    source_path = source_file |> Path.relative_to(File.cwd!()) |> String.replace_leading("./", "")
    source_url = Utils.source_url_pattern(source_url_pattern, source_path, 1)

    %{
      source: source,
      content: content_html,
      group: group,
      id: id,
      source_path: source_path,
      source_url: source_url,
      title: title,
      title_content: title_html || title
    }
  end

  defp build_extra(input, groups, ext, language, autolink_opts, source_url_pattern) do
    build_extra({input, []}, groups, ext, language, autolink_opts, source_url_pattern)
  end

  defp extra_paths(config) do
    Map.new(config.extras, fn
      path when is_binary(path) ->
        base = Path.basename(path)
        {base, Utils.text_to_id(Path.rootname(base))}

      {path, opts} ->
        base = path |> to_string() |> Path.basename()
        {base, opts[:filename] || Utils.text_to_id(Path.rootname(base))}
    end)
  end

  defp disambiguate_id(extra, discriminator) do
    Map.put(extra, :id, "#{extra.id}-#{discriminator}")
  end

  defp sectionize(ast, ".cheatmd") do
    ExDoc.DocAST.sectionize(ast, fn
      {:h2, _, _, _} -> true
      {:h3, _, _, _} -> true
      _ -> false
    end)
  end

  defp sectionize(ast, _), do: ast

  defp filename_to_title(input) do
    input |> Path.basename() |> Path.rootname()
  end

  def filter_list(:module, nodes) do
    Enum.filter(nodes, &(&1.type != :task))
  end

  def filter_list(type, nodes) do
    Enum.filter(nodes, &(&1.type == type))
  end

  def extension_name(input) do
    input
    |> Path.extname()
    |> String.downcase()
  end
end
