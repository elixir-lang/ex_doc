defmodule ExDoc.Formatter.HTML do
  @moduledoc """
  Generate HTML documentation for Elixir projects
  """

  alias __MODULE__.{Assets, Autolink, Templates}
  alias ExDoc.Markdown

  @main "api-reference"

  @doc """
  Generate HTML documentation for the given modules
  """
  @spec run(list, ExDoc.Config.t) :: String.t
  def run(project_nodes, config) when is_map(config) do
    config = normalize_config(config)
    config = %{config | output: Path.expand(config.output)}

    build = Path.join(config.output, ".build")
    output_setup(build, config)

    linked = Autolink.all(project_nodes, ".html", config.deps)
    nodes_map = %{
      modules: filter_list(:module, linked),
      exceptions: filter_list(:exception, linked),
      protocols: filter_list(:protocol, linked),
      tasks: filter_list(:task, linked)
    }

    extras =
      [build_api_reference(nodes_map, config) |
       build_extras(project_nodes, config, ".html")]

    assets_dir = "assets"
    static_files = generate_assets(config, assets_dir, default_assets())

    generated_files =
      generate_sidebar_items(nodes_map, extras, config) ++
      generate_extras(nodes_map, extras, config) ++
      generate_logo(assets_dir, config) ++
      generate_not_found(nodes_map, config) ++
      generate_list(nodes_map.modules, nodes_map, config) ++
      generate_list(nodes_map.exceptions, nodes_map, config) ++
      generate_list(nodes_map.protocols, nodes_map, config) ++
      generate_list(nodes_map.tasks, nodes_map, config) ++
      generate_index(config)

    generate_build(static_files ++ generated_files, build)
    config.output |> Path.join("index.html") |> Path.relative_to_cwd()
  end

  defp normalize_config(%{main: "index"}) do
    raise ArgumentError, message: ~S("main" cannot be set to "index", otherwise it will recursively link to itself)
  end
  defp normalize_config(%{main: main} = config) do
    %{config | main: main || @main}
  end

  defp output_setup(build, config) do
    if File.exists? build do
      build
      |> File.read!
      |> String.split("\n", trim: true)
      |> Enum.map(&Path.join(config.output, &1))
      |> Enum.each(&File.rm/1)
      File.rm build
    else
      File.rm_rf! config.output
      File.mkdir_p! config.output
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

  defp generate_sidebar_items(nodes_map, extras, config) do
    content = Templates.create_sidebar_items(nodes_map, extras)

    digest =
      content
      |> :erlang.md5
      |> Base.encode16(case: :lower)
      |> binary_part(0, 10)

    sidebar_items = "dist/sidebar_items-#{digest}.js"
    File.write!(Path.join(config.output, sidebar_items), content)
    [sidebar_items]
  end

  defp generate_extras(nodes_map, extras, config) do
    Enum.map(extras, fn %{id: id, title: title, content: content} ->
      filename = "#{id}.html"
      output = "#{config.output}/#{filename}"
      config = set_canonical_url(config, filename)
      html = Templates.extra_template(config, title, nodes_map, content)

      if File.regular?(output) do
        IO.puts :stderr, "warning: file #{Path.relative_to_cwd output} already exists"
      end
      File.write!(output, html)
      filename
    end)
  end

  @doc false
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
      File.mkdir(target_dir)
      Enum.map(files, fn {name, content} ->
        target = Path.join(target_dir, name)
        File.write(target, content)
        Path.relative_to(target, config.output)
      end)
    end)
  end

  defp default_assets do
    [{Assets.dist(), "dist"}, {Assets.fonts(), "fonts"}]
  end

  defp build_api_reference(nodes_map, config) do
    api_reference = Templates.api_reference_template(config, nodes_map)
    %{id: "api-reference", title: "API Reference", group: "", content: api_reference}
  end

  @doc """
  Builds extra nodes by normalizing the config entries.
  """
  def build_extras(project_nodes, config, extension) do
    config.extras
    |> Enum.map(&Task.async(fn ->
        build_extra(&1, project_nodes, extension)
       end))
    |> Enum.map(&Task.await(&1, :infinity))
  end

  defp build_extra({input, options}, project_nodes, extension) do
    input = to_string(input)
    id = options[:filename] || input |> input_to_title() |> title_to_id()
    build_extra(input, id, options[:title], options[:group], project_nodes, extension)
  end

  defp build_extra(input, project_nodes, extension) do
    id = input |> input_to_title() |> title_to_id()
    build_extra(input, id, nil, "", project_nodes, extension)
  end

  defp build_extra(input, id, title, group, project_nodes, extension) do
    if valid_extension_name?(input) do
      content =
        input
        |> File.read!()
        |> Autolink.project_doc(project_nodes, nil, extension)

      html_content = Markdown.to_html(content, file: input, line: 1)
      title = title || extract_title(html_content) || input_to_title(input)
      %{id: id, title: title, group: group, content: html_content}
    else
      raise ArgumentError, "file format not recognized, allowed format is: .md"
    end
  end

  def valid_extension_name?(input) do
    file_ext =
      input
      |> Path.extname()
      |> String.downcase()

    if file_ext in [".md"] do
      true
    else
      false
    end
  end

  @tag_regex ~r/<[^>]*>/m
  defp strip_html(header) do
    Regex.replace(@tag_regex, header, "")
  end

  @h1_regex ~r/<h1.*?>(.+)<\/h1>/m
  defp extract_title(content) do
    title = Regex.run(@h1_regex, content, capture: :all_but_first)

    if title do
      title |> List.first() |> strip_html() |> String.trim()
    end
  end

  @doc """
  Convert the input file name into a title_to_filename/1
  """
  def input_to_title(input) do
    input |> Path.basename() |> Path.rootname()
  end

  @doc """
  Creates an ID from a given title
  """
  def title_to_id(title) do
    title |> String.replace(" ", "-") |> String.downcase()
  end

  @doc """
  Generates the logo from config into the given directory
  and adjusts the logo config key.
  """
  def generate_logo(_dir, %{logo: nil}) do
    []
  end
  def generate_logo(dir, %{output: output, logo: logo}) do
    extname =
      logo
      |> Path.extname()
      |> String.downcase()

    if extname in ~w(.png .jpg) do
      filename = Path.join(dir, "logo#{extname}")
      target = Path.join(output, filename)
      File.mkdir_p!(Path.dirname(target))
      File.copy!(logo, target)
      [filename]
    else
      raise ArgumentError, "image format not recognized, allowed formats are: .jpg, .png"
    end
  end

  defp generate_redirect(filename, config, redirect_to) do
    unless File.regular?("#{config.output}/#{redirect_to}") do
      IO.puts :stderr, "warning: #{filename} redirects to #{redirect_to}, which does not exist"
    end
    content = Templates.redirect_template(config, redirect_to)
    File.write!("#{config.output}/#{filename}", content)
  end

  def filter_list(:module, nodes) do
    Enum.filter(nodes, &(not &1.type in [:exception, :protocol, :impl, :task]))
  end

  def filter_list(type, nodes) do
    Enum.filter(nodes, &(&1.type == type))
  end

  defp generate_list(nodes, nodes_map, config) do
    nodes
    |> Enum.map(&Task.async(fn ->
        generate_module_page(&1, nodes_map, config)
       end))
    |> Enum.map(&Task.await(&1, :infinity))
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
end
