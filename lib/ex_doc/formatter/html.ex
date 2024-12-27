defmodule ExDoc.Formatter.HTML do
  @moduledoc false

  alias __MODULE__.{Assets, Templates, SearchData}
  alias ExDoc.{Formatter, Markdown, GroupMatcher, Utils}

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

    # Generate search early on without api reference in extras
    static_files = generate_assets(".", default_assets(config), config)
    search_data = generate_search_data(project_nodes, extras, config)

    # TODO: Move this categorization to the language
    nodes_map = %{
      modules: Formatter.filter_list(:module, project_nodes),
      tasks: Formatter.filter_list(:task, project_nodes)
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
        generate_list(nodes_map.tasks, nodes_map, config) ++
        generate_redirects(config, ".html")

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
        ext = Formatter.extension_name(source_path),
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
          |> Enum.map(&Path.relative_to(&1, output))

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
end
