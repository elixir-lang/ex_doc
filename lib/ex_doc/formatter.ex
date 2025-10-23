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

        docs_groups =
          for group <- node.docs_groups do
            docs =
              for child_node <- group.docs do
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
                render_doc(child_node, language, autolink_opts, opts)
              end

            %{render_doc(group, language, autolink_opts, opts) | docs: docs}
          end

        %{
          render_doc(node, language, [{:id, node.id} | autolink_opts], opts)
          | docs_groups: docs_groups
        }
      end,
      timeout: :infinity
    )
    |> Enum.map(&elem(&1, 1))
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
      |> Enum.map(&normalize_extras/1)
      |> Task.async_stream(
        &build_extra(&1, groups, language, autolink_opts, source_url_pattern),
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

  def filter_list(:module, nodes) do
    Enum.filter(nodes, &(&1.type != :task))
  end

  def filter_list(type, nodes) do
    Enum.filter(nodes, &(&1.type == type))
  end

  # Helper functions

  defp render_doc(%{doc: nil} = node, _language, _autolink_opts, _opts),
    do: node

  defp render_doc(%{doc: doc} = node, language, autolink_opts, opts) do
    doc = autolink_and_highlight(doc, language, autolink_opts, opts)
    %{node | doc: doc}
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

  defp autolink_and_highlight(doc, language, autolink_opts, opts) do
    doc
    |> language.autolink_doc(autolink_opts)
    |> ExDoc.DocAST.highlight(language, opts)
  end

  defp extra_paths(config) do
    Enum.reduce(config.extras, %{}, fn
      path, acc when is_binary(path) ->
        base = Path.basename(path)
        Map.put(acc, base, Utils.text_to_id(Path.rootname(base)))

      {path, opts}, acc ->
        if Keyword.has_key?(opts, :url) do
          acc
        else
          base = path |> to_string() |> Path.basename()

          name =
            Keyword.get_lazy(opts, :filename, fn -> Utils.text_to_id(Path.rootname(base)) end)

          Map.put(acc, base, name)
        end
    end)
  end

  defp normalize_extras(base) when is_binary(base), do: {base, %{}}
  defp normalize_extras({base, opts}), do: {base, Map.new(opts)}

  defp disambiguate_id(extra, discriminator) do
    Map.put(extra, :id, "#{extra.id}-#{discriminator}")
  end

  defp build_extra({input, %{url: _} = input_options}, groups, _lang, _auto, _url_pattern) do
    input = to_string(input)
    title = validate_extra_string!(input_options, :title) || input
    url = validate_extra_string!(input_options, :url)
    group = GroupMatcher.match_extra(groups, url)
    %{group: group, id: Utils.text_to_id(title), title: title, url: url}
  end

  defp build_extra({input, input_options}, groups, language, autolink_opts, source_url_pattern) do
    input = to_string(input)

    id =
      validate_extra_string!(input_options, :filename) ||
        input |> filename_to_title() |> Utils.text_to_id()

    source_file = validate_extra_string!(input_options, :source) || input
    opts = [file: source_file, line: 1]

    {extension, source, ast} =
      case extension_name(input) do
        extension when extension in ["", ".txt"] ->
          source = File.read!(input)
          ast = [{:pre, [], ["\n" <> source], %{}}]
          {extension, source, ast}

        extension when extension in [".md", ".livemd", ".cheatmd"] ->
          source = File.read!(input)

          ast =
            source
            |> Markdown.to_ast(opts)
            |> ExDoc.DocAST.add_ids_to_headers([:h2, :h3])
            |> autolink_and_highlight(language, [file: input] ++ autolink_opts, opts)

          {extension, source, ast}

        _ ->
          raise ArgumentError,
                "file extension not recognized, allowed extension is either .cheatmd, .livemd, .md, .txt or no extension"
      end

    {title_doc, title_text, ast} =
      case ExDoc.DocAST.extract_title(ast) do
        {:ok, title_doc, ast} -> {title_doc, ExDoc.DocAST.text(title_doc), ast}
        :error -> {nil, nil, ast}
      end

    title =
      validate_extra_string!(input_options, :title) || title_text || filename_to_title(input)

    group = GroupMatcher.match_extra(groups, input)
    source_path = source_file |> Path.relative_to(File.cwd!()) |> String.replace_leading("./", "")
    source_url = source_url_pattern.(source_path, 1)
    search_data = validate_search_data!(input_options[:search_data])

    %{
      type: extra_type(extension),
      source: source,
      group: group,
      id: id,
      doc: ast,
      source_path: source_path,
      source_url: source_url,
      search_data: search_data,
      title: title,
      title_doc: title_doc || title
    }
  end

  defp validate_extra_string!(input_options, key) do
    case input_options[key] do
      nil ->
        nil

      binary when is_binary(binary) ->
        binary

      other ->
        raise ArgumentError,
              "extra field #{inspect(key)} must be a string, got: #{inspect(other)}"
    end
  end

  @search_data_keys [:anchor, :body, :title, :type]

  defp validate_search_data!(nil), do: nil

  defp validate_search_data!(search_data) when is_list(search_data) do
    Enum.each(search_data, fn search_data ->
      has_keys = Map.keys(search_data)

      if Enum.sort(has_keys) != @search_data_keys do
        raise ArgumentError,
              "expected search data to be a list of maps with the keys: #{inspect(@search_data_keys)}, " <>
                "found keys: #{inspect(has_keys)}"
      end
    end)

    search_data
  end

  defp validate_search_data!(search_data) do
    raise ArgumentError,
          "expected search data to be a list of maps with the keys: #{inspect(@search_data_keys)}, " <>
            "found: #{inspect(search_data)}"
  end

  defp extension_name(input) do
    input
    |> Path.extname()
    |> String.downcase()
  end

  defp filename_to_title(input) do
    input |> Path.basename() |> Path.rootname()
  end

  defp extra_type(".cheatmd"), do: :cheatmd
  defp extra_type(".livemd"), do: :livemd
  defp extra_type(_), do: :extra

  @doc """
  Generate assets from configs with the given default assets.
  """
  def generate_assets(namespace, defaults, %{output: output, assets: assets}) do
    namespaced_assets =
      if is_map(assets) do
        Enum.map(assets, fn {source, target} -> {source, Path.join(namespace, target)} end)
      else
        IO.warn("""
        giving a binary to :assets is deprecated, please give a map from source to target instead:

            #{inspect(assets: %{assets => "assets"})}
        """)

        [{assets, Path.join(namespace, "assets")}]
      end

    Enum.flat_map(defaults ++ namespaced_assets, fn {dir_or_files, relative_target_dir} ->
      target_dir = Path.join(output, relative_target_dir)
      File.mkdir_p!(target_dir)

      cond do
        is_list(dir_or_files) ->
          Enum.map(dir_or_files, fn {name, content} ->
            target = Path.join(target_dir, name)
            File.write(target, content)
            Path.relative_to(target, output)
          end)

        is_binary(dir_or_files) and File.dir?(dir_or_files) ->
          dir_or_files
          |> File.cp_r!(target_dir, dereference_symlinks: true)
          |> Enum.reduce([], fn path, acc ->
            # Omit directories in .build file
            if File.dir?(path) do
              acc
            else
              [Path.relative_to(path, output) | acc]
            end
          end)
          |> Enum.reverse()

        is_binary(dir_or_files) ->
          []

        true ->
          raise ":assets must be a map of source directories to target directories"
      end
    end)
  end

  @doc """
  Generates the logo from config into the given directory.
  """
  def generate_logo(_dir, %{logo: nil}) do
    []
  end

  def generate_logo(dir, %{output: output, logo: logo}) do
    generate_image(output, dir, logo, "logo")
  end

  @doc """
  Generates the cover from config into the given directory.
  """
  def generate_cover(_dir, %{cover: nil}) do
    []
  end

  def generate_cover(dir, %{output: output, cover: cover}) do
    generate_image(output, dir, cover, "cover")
  end

  def generate_image(output, dir, image, name) do
    extname =
      image
      |> Path.extname()
      |> String.downcase()

    if extname in ~w(.png .jpg .jpeg .svg) do
      filename = Path.join(dir, "#{name}#{extname}")
      target = Path.join(output, filename)
      File.mkdir_p!(Path.dirname(target))
      File.copy!(image, target)
      [filename]
    else
      raise ArgumentError, "image format not recognized, allowed formats are: .png, .jpg, .svg"
    end
  end
end
