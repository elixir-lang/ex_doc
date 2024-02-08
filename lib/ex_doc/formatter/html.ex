defmodule ExDoc.Formatter.HTML do
  @moduledoc false

  alias __MODULE__.{Assets, Templates, SearchData}
  alias ExDoc.{Markdown, GroupMatcher, Utils}

  @main "api-reference"
  @assets_dir "assets"

  @doc """
  Generate HTML documentation for the given modules.
  """
  @spec run([ExDoc.ModuleNode.t()], [ExDoc.ModuleNode.t()], ExDoc.Config.t()) :: String.t()
  def run(project_nodes, filtered_modules, config) when is_map(config) do
    config = normalize_config(config)
    config = %{config | output: Path.expand(config.output)}

    build = Path.join(config.output, ".build")
    output_setup(build, config)

    project_nodes = render_all(project_nodes, filtered_modules, ".html", config, [])
    extras = build_extras(config, ".html")

    # Generate search early on without api reference in extras
    static_files = generate_assets(config, @assets_dir, default_assets(config))
    search_data = generate_search_data(project_nodes, extras, config)

    # TODO: Move this categorization to the language
    nodes_map = %{
      modules: filter_list(:module, project_nodes),
      tasks: filter_list(:task, project_nodes)
    }

    extras =
      if config.api_reference do
        [build_api_reference(nodes_map, config) | extras]
      else
        extras
      end

    all_files =
      search_data ++
        static_files ++
        generate_sidebar_items(nodes_map, extras, config) ++
        generate_extras(nodes_map, extras, config) ++
        generate_logo(@assets_dir, config) ++
        generate_search(nodes_map, config) ++
        generate_not_found(nodes_map, config) ++
        generate_list(nodes_map.modules, nodes_map, config) ++
        generate_list(nodes_map.tasks, nodes_map, config) ++ generate_index(config)

    generate_build(Enum.sort(all_files), build)
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
                  current_kfa: {:function, child_node.name, child_node.arity}
                ]

            specs = Enum.map(child_node.specs, &language.autolink_spec(&1, autolink_opts))
            child_node = %{child_node | specs: specs}
            render_doc(child_node, language, autolink_opts, opts)
          end

        typespecs =
          for child_node <- node.typespecs do
            id = id(node, child_node)

            autolink_opts =
              autolink_opts ++
                [
                  id: id,
                  line: child_node.doc_line,
                  file: child_node.doc_file,
                  current_kfa: {child_node.type, child_node.name, child_node.arity}
                ]

            child_node = %{
              child_node
              | spec: language.autolink_spec(child_node.spec, autolink_opts)
            }

            render_doc(child_node, language, autolink_opts, opts)
          end

        %{
          render_doc(node, language, [{:id, node.id} | autolink_opts], opts)
          | docs: docs,
            typespecs: typespecs
        }
      end,
      timeout: :infinity
    )
    |> Enum.map(&elem(&1, 1))
  end

  defp render_doc(%{doc: nil} = node, _language, _autolink_opts, _opts),
    do: node

  defp render_doc(%{doc: doc} = node, language, autolink_opts, opts) do
    rendered = autolink_and_render(doc, language, autolink_opts, opts)
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

  defp autolink_and_render(doc, language, autolink_opts, opts) do
    doc
    |> language.autolink_doc(autolink_opts)
    |> ExDoc.DocAST.to_string()
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
    entries = Enum.map(files, &[&1, "\n"])
    File.write!(build, entries)
  end

  defp generate_index(config) do
    index_file = "index.html"
    main_file = "#{config.main}.html"
    generate_redirect(index_file, config, main_file)
    [index_file]
  end

  defp generate_not_found(nodes_map, config) do
    filename = "404.html"
    config = set_canonical_url(config, filename)
    content = Templates.not_found_template(config, nodes_map)
    File.write!("#{config.output}/#{filename}", content)
    [filename]
  end

  defp generate_search(nodes_map, config) do
    filename = "search.html"
    config = set_canonical_url(config, filename)
    content = Templates.search_template(config, nodes_map)
    File.write!("#{config.output}/#{filename}", content)
    [filename]
  end

  defp generate_sidebar_items(nodes_map, extras, config) do
    content = Templates.create_sidebar_items(nodes_map, extras)
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

  defp generate_extras(nodes_map, extras, config) do
    generated_extras =
      extras
      |> with_prev_next()
      |> Enum.map(fn {node, prev, next} ->
        filename = "#{node.id}.html"
        output = "#{config.output}/#{filename}"
        config = set_canonical_url(config, filename)

        refs = %{
          prev: prev && %{path: "#{prev.id}.html", title: prev.title},
          next: next && %{path: "#{next.id}.html", title: next.title}
        }

        extension = node.source_path && Path.extname(node.source_path)
        html = Templates.extra_template(config, node, extra_type(extension), nodes_map, refs)

        if File.regular?(output) do
          ExDoc.Utils.warn("warning: file #{Path.relative_to_cwd(output)} already exists", [])
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
  def generate_assets(config, assets_dir, defaults) do
    write_default_assets(config, defaults) ++ copy_assets(config, assets_dir)
  end

  defp copy_assets(config, assets_dir) do
    if path = config.assets do
      path
      |> Path.join("**/*")
      |> Path.wildcard()
      |> Enum.map(fn source ->
        filename = Path.join(assets_dir, Path.relative_to(source, path))
        target = Path.join(config.output, filename)
        File.mkdir(Path.dirname(target))
        File.copy(source, target)
        filename
      end)
    else
      []
    end
  end

  defp write_default_assets(config, sources) do
    Enum.flat_map(sources, fn {files, dir} ->
      target_dir = Path.join(config.output, dir)
      File.mkdir_p!(target_dir)

      Enum.map(files, fn {name, content} ->
        target = Path.join(target_dir, name)
        File.write(target, content)
        Path.relative_to(target, config.output)
      end)
    end)
  end

  defp default_assets(config) do
    [
      {Assets.dist(config.proglang), "dist"},
      {Assets.fonts(), "dist"}
    ]
  end

  defp build_api_reference(nodes_map, config) do
    api_reference = Templates.api_reference_template(nodes_map)

    title_content =
      ~s{API Reference <small class="app-vsn">#{config.project} v#{config.version}</small>}

    %{
      content: api_reference,
      group: nil,
      id: "api-reference",
      source_path: nil,
      source_url: config.source_url,
      title: "API Reference",
      title_content: title_content
    }
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
    |> Enum.sort_by(fn extra -> GroupMatcher.group_index(groups, extra.group) end)
  end

  defp disambiguate_id(extra, discriminator) do
    Map.put(extra, :id, "#{extra.id}-#{discriminator}")
  end

  defp build_extra({input, options}, groups, language, autolink_opts, source_url_pattern) do
    input = to_string(input)
    id = options[:filename] || input |> filename_to_title() |> text_to_id()
    source = options[:source] || input

    build_extra(
      input,
      id,
      options[:title],
      groups,
      language,
      autolink_opts,
      source,
      source_url_pattern
    )
  end

  defp build_extra(input, groups, language, autolink_opts, source_url_pattern) do
    id = input |> filename_to_title() |> text_to_id()

    build_extra(
      input,
      id,
      nil,
      groups,
      language,
      autolink_opts,
      input,
      source_url_pattern
    )
  end

  defp build_extra(
         input,
         id,
         title,
         groups,
         language,
         autolink_opts,
         source_file,
         source_url_pattern
       ) do
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

    content_html = autolink_and_render(ast, language, [file: input] ++ autolink_opts, opts)

    group = GroupMatcher.match_extra(groups, input)
    title = title || title_text || filename_to_title(input)

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

  defp extension_name(input) do
    input
    |> Path.extname()
    |> String.downcase()
  end

  defp sectionize(ast, ".cheatmd") do
    ExDoc.DocAST.sectionize(ast, fn
      {:h2, _, _, _} -> true
      {:h3, _, _, _} -> true
      _ -> false
    end)
  end

  defp sectionize(ast, _), do: ast

  @doc """
  Convert the input file name into a title
  """
  def filename_to_title(input) do
    input |> Path.basename() |> Path.rootname()
  end

  @clean_html_regex ~r/<\/?\s*[a-zA-Z]+(?:[^>=]|='[^']*'|="[^"]*"|=[^'"][^\s>]*)*>/

  @doc """
  Strips html tags from text leaving their text content
  """
  def strip_tags(text, replace_with \\ "") when is_binary(text) do
    String.replace(text, @clean_html_regex, replace_with)
  end

  @doc """
  Generates an ID from some text

  Used primarily with titles, headings, and functions group names.
  """
  def text_to_id(atom) when is_atom(atom), do: text_to_id(Atom.to_string(atom))

  def text_to_id(text) when is_binary(text) do
    text
    |> strip_tags()
    |> String.replace(~r/&#\d+;/, "")
    |> String.replace(~r/&[A-Za-z0-9]+;/, "")
    |> String.replace(~r/\W+/u, "-")
    |> String.trim("-")
    |> String.downcase()
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
      ExDoc.Utils.warn("#{filename} redirects to #{redirect_to}, which does not exist", [])
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

  defp generate_list(nodes, nodes_map, config) do
    nodes
    |> Task.async_stream(&generate_module_page(&1, nodes_map, config), timeout: :infinity)
    |> Enum.map(&elem(&1, 1))
  end

  defp generate_module_page(module_node, nodes_map, config) do
    filename = "#{module_node.id}.html"
    config = set_canonical_url(config, filename)
    content = Templates.module_page(module_node, nodes_map, config)
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
    Map.new(config.extras, fn
      path when is_binary(path) ->
        base = Path.basename(path)
        {base, text_to_id(Path.rootname(base))}

      {path, opts} ->
        base = path |> to_string() |> Path.basename()
        {base, opts[:filename] || text_to_id(Path.rootname(base))}
    end)
  end
end
