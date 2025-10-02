defmodule ExDoc.Formatter.HTML do
  @moduledoc false

  alias __MODULE__.{Assets, Templates, SearchData}
  alias ExDoc.{Formatter, Utils}

  @main "api-reference"
  @assets_dir "assets"

  @doc """
  Generates HTML documentation for the given modules.
  """
  @spec run([ExDoc.ModuleNode.t()], [ExDoc.ModuleNode.t()], ExDoc.Config.t()) :: String.t()
  def run(project_nodes, filtered_modules, config) when is_map(config) do
    config = normalize_config(config)
    config = %{config | output: Path.expand(config.output)}

    build = Path.join(config.output, ".build")
    output_setup(build, config)

    project_nodes = Formatter.render_all(project_nodes, filtered_modules, ".html", config, [])
    extras = Formatter.build_extras(config, ".html")

    static_files = Formatter.generate_assets(".", default_assets(config), config)
    search_data = generate_search_data(project_nodes, extras, config)

    # TODO: Move this categorization to the language
    nodes_map = %{
      modules: Formatter.filter_list(:module, project_nodes),
      tasks: Formatter.filter_list(:task, project_nodes)
    }

    all_files =
      search_data ++
        static_files ++
        generate_sidebar_items(nodes_map, extras, config) ++
        generate_api_reference(nodes_map, config) ++
        generate_extras(extras, config) ++
        generate_favicon(@assets_dir, config) ++
        Formatter.generate_logo(@assets_dir, config) ++
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

  defp extension_name(input) do
    input
    |> Path.extname()
    |> String.downcase()
  end

  @doc """
  Generates the favicon from config into the given directory.
  """
  def generate_favicon(_dir, %{favicon: nil}) do
    []
  end

  def generate_favicon(dir, %{output: output, favicon: favicon}) do
    Formatter.generate_image(output, dir, favicon, "favicon")
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
end
