defmodule ExDoc.Formatter.HTML do
  @moduledoc false

  alias __MODULE__.{Assets, Templates, SearchData}
  alias ExDoc.{Markdown, GroupMatcher, Utils}

  @main "api-reference"
  @assets_dir "assets"
  @search_data_keys [:anchor, :body, :title, :type]

  @doc """
  Generates HTML documentation for the given modules.
  """
  @spec run([ExDoc.ModuleNode.t()], [ExDoc.ModuleNode.t()], ExDoc.Config.t()) :: String.t()
  def run(project_nodes, filtered_modules, config) when is_map(config) do
    config = normalize_config(config)
    config = %{config | output: Path.expand(config.output)}

    build = Path.join(config.output, ".build")
    output_setup(build, config)

    project_nodes = render_all(project_nodes, filtered_modules, ".html", config, [])
    extras = build_extras(config, ".html")

    static_files = generate_assets(".", default_assets(config), config)
    search_data = generate_search_data(project_nodes, extras, config)

    # TODO: Move this categorization to the language
    nodes_map = %{
      modules: filter_list(:module, project_nodes),
      tasks: filter_list(:task, project_nodes)
    }

    all_files =
      search_data ++
        static_files ++
        generate_sidebar_items(nodes_map, extras, config) ++
        generate_api_reference(nodes_map, config) ++
        generate_extras(extras, config) ++
        generate_favicon(@assets_dir, config) ++
        generate_logo(@assets_dir, config) ++
        generate_search(config) ++
        generate_not_found(config) ++
        generate_list(nodes_map.modules, config) ++
        generate_list(nodes_map.tasks, config) ++
        generate_redirects(config, ".html")

    generate_build(all_files, build)
    config.output |> Path.join("index.html") |> Path.relative_to_cwd()
  end

  defp normalize_config(%{main: "index"}) do
    raise ArgumentError,
      message: ~S("main" cannot be set to "index", otherwise it will recursively link to itself)
  end

  defp normalize_config(%{main: main} = config) do
    %{config | main: main || @main}
  end

  @doc """
  Autolinks and renders all docs.
  """
  # TODO: Move this to normalize_doc_ast in the retriever
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
            render_doc(child_node, language, autolink_opts, opts)
          end

        %{
          render_doc(node, language, [{:id, node.id} | autolink_opts], opts)
          | docs: docs
        }
      end,
      timeout: :infinity
    )
    |> Enum.map(&elem(&1, 1))
  end

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

  defp output_setup(build, config) do
    if File.exists?(build) do
      build
      |> File.read!()
      |> String.split("\n", trim: true)
      |> Enum.map(&Path.join(config.output, &1))
      |> Enum.each(&File.rm/1)

      File.rm(build)
    else
      File.rm_rf!(config.output)
      File.mkdir_p!(config.output)
    end
  end

  defp generate_build(files, build) do
    entries =
      files
      |> Enum.uniq()
      |> Enum.sort()
      |> Enum.map(&[&1, "\n"])

    File.write!(build, entries)
  end

  defp generate_not_found(config) do
    filename = "404.html"
    config = set_canonical_url(config, filename)
    content = Templates.not_found_template(config)
    File.write!("#{config.output}/#{filename}", content)
    [filename]
  end

  defp generate_search(config) do
    filename = "search.html"
    config = set_canonical_url(config, filename)
    content = Templates.search_template(config)
    File.write!("#{config.output}/#{filename}", content)
    [filename]
  end

  defp generate_sidebar_items(nodes_map, extras, config) do
    content = Templates.create_sidebar_items(config, nodes_map, extras)

    path = "dist/sidebar_items-#{digest(content)}.js"
    File.write!(Path.join(config.output, path), content)
    [path]
  end

  defp generate_search_data(linked, extras, config) do
    content = SearchData.create(linked, extras, config.proglang)
    path = "dist/search_data-#{digest(content)}.js"
    File.write!(Path.join(config.output, path), content)
    [path]
  end

  defp digest(content) do
    content
    |> :erlang.md5()
    |> Base.encode16(case: :upper)
    |> binary_part(0, 8)
  end

  defp generate_extras(extras, config) do
    generated_extras =
      extras
      |> Enum.reject(&is_map_key(&1, :url))
      |> with_prev_next()
      |> Enum.map(fn {node, prev, next} ->
        filename = "#{node.id}.html"
        output = "#{config.output}/#{filename}"
        config = set_canonical_url(config, filename)

        refs = %{
          prev: prev && %{path: "#{prev.id}.html", title: prev.title},
          next: next && %{path: "#{next.id}.html", title: next.title}
        }

        html = Templates.extra_template(config, node, refs)

        if File.regular?(output) do
          Utils.warn("file #{Path.relative_to_cwd(output)} already exists", [])
        end

        File.write!(output, html)
        filename
      end)

    generated_extras ++ copy_extras(config, extras)
  end

  defp extra_type(".cheatmd"), do: :cheatmd
  defp extra_type(".livemd"), do: :livemd
  defp extra_type(_), do: :extra

  defp copy_extras(config, extras) do
    for %{source_path: source_path, id: id} when source_path != nil <- extras,
        ext = extension_name(source_path),
        ext == ".livemd" do
      output = "#{config.output}/#{id}#{ext}"

      File.copy!(source_path, output)

      output
    end
  end

  defp with_prev_next([]), do: []

  defp with_prev_next([head | tail]) do
    Enum.zip([[head | tail], [nil, head | tail], tail ++ [nil]])
  end

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

  defp default_assets(config) do
    [
      {Assets.dist(config.proglang), "dist"},
      {Assets.fonts(), "dist"}
    ]
  end

  defp generate_api_reference(_nodes_map, %{api_reference: false}) do
    []
  end

  defp generate_api_reference(nodes_map, config) do
    filename = "api-reference.html"
    output = "#{config.output}/#{filename}"
    config = set_canonical_url(config, filename)

    html = Templates.api_reference_template(config, nodes_map)

    if File.regular?(output) do
      Utils.warn("file #{Path.relative_to_cwd(output)} already exists", [])
    end

    File.write!(output, html)
    [filename]
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

  def generate_redirects(config, ext) do
    config.redirects
    |> Map.new()
    |> Map.put_new("index", config.main)
    |> Enum.map(fn {from, to} ->
      unless is_binary(from),
        do: raise("expected a string for the source of a redirect, got: #{inspect(from)}")

      unless is_binary(to),
        do: raise("expected a string for the destination of a redirect, got: #{inspect(to)}")

      source = from <> ext
      destination = to <> ext
      generate_redirect(source, config, destination)

      source
    end)
  end

  defp normalize_extras(base) when is_binary(base), do: {base, %{}}
  defp normalize_extras({base, opts}), do: {base, Map.new(opts)}

  defp disambiguate_id(extra, discriminator) do
    Map.put(extra, :id, "#{extra.id}-#{discriminator}")
  end

  defp build_extra({input, %{url: _} = input_options}, groups, _lang, _auto, _url_pattern) do
    input = to_string(input)
    title = input_options[:title] || input
    group = GroupMatcher.match_extra(groups, input_options[:url])

    %{group: group, id: Utils.text_to_id(title), title: title, url: input_options[:url]}
  end

  defp build_extra({input, input_options}, groups, language, autolink_opts, source_url_pattern) do
    input = to_string(input)
    id = input_options[:filename] || input |> filename_to_title() |> Utils.text_to_id()
    source_file = input_options[:source] || input
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

    title = input_options[:title] || title_text || filename_to_title(input)
    group = GroupMatcher.match_extra(groups, input)
    source_path = source_file |> Path.relative_to(File.cwd!()) |> String.replace_leading("./", "")
    source_url = source_url_pattern.(source_path, 1)
    search_data = normalize_search_data!(input_options[:search_data])

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

  defp normalize_search_data!(nil), do: nil

  defp normalize_search_data!(search_data) when is_list(search_data) do
    Enum.each(search_data, fn search_data ->
      has_keys = Map.keys(search_data)

      if Enum.sort(has_keys) != @search_data_keys do
        raise ArgumentError,
              "Expected search data to be a list of maps with the keys: #{inspect(@search_data_keys)}, found keys: #{inspect(has_keys)}"
      end
    end)

    search_data
  end

  defp normalize_search_data!(search_data) do
    raise ArgumentError,
          "Expected search data to be a list of maps with the keys: #{inspect(@search_data_keys)}, found: #{inspect(search_data)}"
  end

  defp extension_name(input) do
    input
    |> Path.extname()
    |> String.downcase()
  end

  defp filename_to_title(input) do
    input |> Path.basename() |> Path.rootname()
  end

  @doc """
  Generates the favicon from config into the given directory.
  """
  def generate_favicon(_dir, %{favicon: nil}) do
    []
  end

  def generate_favicon(dir, %{output: output, favicon: favicon}) do
    generate_image(output, dir, favicon, "favicon")
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

  defp generate_image(output, dir, image, name) do
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

  defp generate_redirect(filename, config, redirect_to) do
    unless case_sensitive_file_regular?("#{config.output}/#{redirect_to}") do
      Utils.warn("#{filename} redirects to #{redirect_to}, which does not exist", [])
    end

    content = Templates.redirect_template(config, redirect_to)
    File.write!("#{config.output}/#{filename}", content)
  end

  defp case_sensitive_file_regular?(path) do
    if File.regular?(path) do
      files = path |> Path.dirname() |> File.ls!()
      Path.basename(path) in files
    else
      false
    end
  end

  def filter_list(:module, nodes) do
    Enum.filter(nodes, &(&1.type != :task))
  end

  def filter_list(type, nodes) do
    Enum.filter(nodes, &(&1.type == type))
  end

  defp generate_list(nodes, config) do
    nodes
    |> Task.async_stream(&generate_module_page(&1, config), timeout: :infinity)
    |> Enum.map(&elem(&1, 1))
  end

  defp generate_module_page(module_node, config) do
    filename = "#{module_node.id}.html"
    config = set_canonical_url(config, filename)
    content = Templates.module_page(module_node, config)
    File.write!("#{config.output}/#{filename}", content)
    filename
  end

  defp set_canonical_url(config, filename) do
    if config.canonical do
      canonical_url =
        config.canonical
        |> String.trim_trailing("/")
        |> Kernel.<>("/" <> filename)

      Map.put(config, :canonical, canonical_url)
    else
      config
    end
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
end
