defmodule ExDoc.Formatter.HTML do
  @moduledoc false

  alias __MODULE__.{Assets, Templates, SearchItems}
  alias ExDoc.{Autolink, Markdown, GroupMatcher}

  @main "api-reference"
  @assets_dir "assets"

  @doc """
  Generate HTML documentation for the given modules.
  """
  @spec run(list, ExDoc.Config.t()) :: String.t()
  def run(project_nodes, config) when is_map(config) do
    config = normalize_config(config)
    config = %{config | output: Path.expand(config.output)}

    build = Path.join(config.output, ".build")
    output_setup(build, config)

    project_nodes = render_all(project_nodes, ".html", config, [])
    extras = build_extras(config, ".html")

    # Generate search early on without api reference in extras
    static_files = generate_assets(config, @assets_dir, default_assets(config))
    search_items = generate_search_items(project_nodes, extras, config)

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
      search_items ++
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
  def render_all(project_nodes, ext, config, opts) do
    project_nodes
    |> Enum.map(fn node ->
      autolink_opts = [
        apps: config.apps,
        current_module: node.module,
        ext: ext,
        extras: extra_paths(config),
        skip_undefined_reference_warnings_on: config.skip_undefined_reference_warnings_on,
        module_id: node.id,
        file: node.source_path,
        line: node.doc_line,
        deps: config.deps
      ]

      docs =
        for child_node <- node.docs do
          id = id(node, child_node)
          autolink_opts = autolink_opts ++ [id: id, line: child_node.doc_line]
          specs = Enum.map(child_node.specs, &Autolink.typespec(&1, autolink_opts))
          child_node = %{child_node | specs: specs}
          render_doc(child_node, autolink_opts, opts)
        end

      typespecs =
        for child_node <- node.typespecs do
          id = id(node, child_node)
          autolink_opts = autolink_opts ++ [id: id, line: child_node.doc_line]
          child_node = %{child_node | spec: Autolink.typespec(child_node.spec, autolink_opts)}
          render_doc(child_node, autolink_opts, opts)
        end

      id = id(node, nil)
      %{render_doc(node, [{:id, id} | autolink_opts], opts) | docs: docs, typespecs: typespecs}
    end)
  end

  defp render_doc(%{doc: nil} = node, _autolink_opts, _opts),
    do: node

  defp render_doc(%{doc: doc} = node, autolink_opts, opts) do
    rendered = autolink_and_render(doc, autolink_opts, opts)
    %{node | rendered_doc: rendered}
  end

  defp id(%{id: id}, nil), do: id
  defp id(%{id: mod_id}, %ExDoc.FunctionNode{id: id, type: :callback}), do: "c:#{mod_id}.#{id}"
  defp id(%{id: mod_id}, %ExDoc.FunctionNode{id: id}), do: "#{mod_id}.#{id}"
  defp id(%{id: mod_id}, %ExDoc.TypeNode{id: id}), do: "t:#{mod_id}.#{id}"

  defp autolink_and_render(doc, autolink_opts, opts) do
    doc
    |> Autolink.doc(autolink_opts)
    |> ast_to_html()
    |> IO.iodata_to_binary()
    |> ExDoc.Highlighter.highlight_code_blocks(opts)
  end

  @doc false
  def ast_to_html(list) when is_list(list), do: Enum.map(list, &ast_to_html/1)
  def ast_to_html(binary) when is_binary(binary), do: Templates.h(binary)

  def ast_to_html({tag, attrs, ast}) do
    attrs = Enum.map(attrs, fn {key, val} -> " #{key}=\"#{val}\"" end)
    ["<#{tag}", attrs, ">", ast_to_html(ast), "</#{tag}>"]
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
    sidebar_items = "dist/sidebar_items-#{digest(content)}.js"
    File.write!(Path.join(config.output, sidebar_items), content)
    [sidebar_items]
  end

  defp generate_search_items(linked, extras, config) do
    content = SearchItems.create(linked, extras)
    search_items = "dist/search_items-#{digest(content)}.js"
    File.write!(Path.join(config.output, search_items), content)
    [search_items]
  end

  defp digest(content) do
    content
    |> :erlang.md5()
    |> Base.encode16(case: :lower)
    |> binary_part(0, 10)
  end

  defp generate_extras(nodes_map, extras, config) do
    Enum.map(extras, fn %{id: id, title: title, content: content} ->
      filename = "#{id}.html"
      output = "#{config.output}/#{filename}"
      config = set_canonical_url(config, filename)
      html = Templates.extra_template(config, title, nodes_map, content)

      if File.regular?(output) do
        IO.puts(:stderr, "warning: file #{Path.relative_to_cwd(output)} already exists")
      end

      File.write!(output, html)
      filename
    end)
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
      {Assets.fonts(), "dist/html/fonts"}
    ]
  end

  defp build_api_reference(nodes_map, config) do
    api_reference = Templates.api_reference_template(config, nodes_map)
    %{id: "api-reference", title: "API Reference", group: "", content: api_reference}
  end

  @doc """
  Builds extra nodes by normalizing the config entries.
  """
  def build_extras(config, ext) do
    groups = config.groups_for_extras

    config.extras
    |> Task.async_stream(&build_extra(&1, groups, config, ext), timeout: :infinity)
    |> Enum.map(&elem(&1, 1))
    |> Enum.sort_by(fn extra -> GroupMatcher.group_index(groups, extra.group) end)
  end

  defp build_extra({input, options}, groups, config, ext) do
    input = to_string(input)
    id = options[:filename] || input |> filename_to_title() |> text_to_id()
    build_extra(input, id, options[:title], groups, config, ext)
  end

  defp build_extra(input, groups, config, ext) do
    id = input |> filename_to_title() |> text_to_id()
    build_extra(input, id, nil, groups, config, ext)
  end

  defp build_extra(input, id, title, groups, config, ext) do
    autolink_opts = [
      apps: config.apps,
      file: input,
      ext: ext,
      extras: extra_paths(config),
      skip_undefined_reference_warnings_on: config.skip_undefined_reference_warnings_on,
      deps: config.deps
    ]

    opts = [file: input, line: 1]

    ast =
      case extension_name(input) do
        extension when extension in ["", ".txt"] ->
          [{:pre, [], "\n" <> File.read!(input)}]

        ".md" ->
          input
          |> File.read!()
          |> Markdown.to_ast(opts)

        _ ->
          raise ArgumentError,
                "file extension not recognized, allowed extension is either .md, .txt or no extension"
      end

    html_content = autolink_and_render(ast, autolink_opts, opts)

    group = GroupMatcher.match_extra(groups, input)
    title = title || extract_title(html_content) || filename_to_title(input)

    %{id: id, title: title, group: group, content: html_content}
  end

  defp extension_name(input) do
    input
    |> Path.extname()
    |> String.downcase()
  end

  @tag_regex ~r/<[^>]*>/m
  defp strip_html(header) do
    Regex.replace(@tag_regex, header, "")
  end

  @h1_regex ~r/<h1.*?>(.+?)<\/h1>/m
  defp extract_title(content) do
    title = Regex.run(@h1_regex, content, capture: :all_but_first)

    if title do
      title |> List.first() |> strip_html() |> String.trim()
    end
  end

  @doc """
  Convert the input file name into a title
  """
  def filename_to_title(input) do
    input |> Path.basename() |> Path.rootname()
  end

  @clean_html_regex ~r/<(?:[^>=]|='[^']*'|="[^"]*"|=[^'"][^\s>]*)*>/

  @doc """
  Strips html tags from text leaving their text content
  """
  def strip_tags(text) when is_binary(text) do
    String.replace(text, @clean_html_regex, "")
  end

  @doc """
  Generates an ID from some text

  Used primarily with titles, headings and functions group names.
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
  Generate a link id for the given node.
  """
  def link_id(node), do: link_id(node.id, node.type)

  @doc """
  Generate a link id for the given id and type.
  """
  def link_id(id, type) do
    case type do
      :macrocallback -> "c:#{id}"
      :callback -> "c:#{id}"
      :type -> "t:#{id}"
      :opaque -> "t:#{id}"
      _ -> "#{id}"
    end
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

    if extname in ~w(.png .jpg .svg) do
      filename = Path.join(dir, "#{name}#{extname}")
      target = Path.join(output, filename)
      File.mkdir_p!(Path.dirname(target))
      File.copy!(image, target)
      [filename]
    else
      raise ArgumentError, "image format not recognized, allowed formats are: .jpg, .png"
    end
  end

  defp generate_redirect(filename, config, redirect_to) do
    unless case_sensitive_file_regular?("#{config.output}/#{redirect_to}") do
      IO.puts(:stderr, "warning: #{filename} redirects to #{redirect_to}, which does not exist")
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
        |> Path.join(filename)

      Map.put(config, :canonical, canonical_url)
    else
      config
    end
  end

  defp extra_paths(config) do
    Enum.map(config.extras, fn
      path when is_binary(path) ->
        Path.basename(path)

      {path, _} ->
        path |> Atom.to_string() |> Path.basename()
    end)
  end
end
