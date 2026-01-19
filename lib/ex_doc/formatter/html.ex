defmodule ExDoc.Formatter.HTML do
  @moduledoc false

  alias __MODULE__.{Assets, Templates, SearchData}
  alias ExDoc.Formatter

  @assets_dir "assets"

  def autolink_options do
    [extension: ".html"]
  end

  def run(config, project_nodes, extras) when is_map(config) do
    if config.main == "index" do
      raise ArgumentError,
            ~S("main" cannot be set to "index", otherwise it will recursively link to itself)
    end

    File.mkdir_p!(config.output)

    static_files =
      Formatter.copy_assets(config.assets, config.output) ++
        Formatter.copy_assets(additional_assets(config), config.output)

    search_data = generate_search_data(project_nodes, extras, config)

    {modules, tasks} = Enum.split_with(project_nodes, &(&1.type != :task))

    all_files =
      search_data ++
        static_files ++
        generate_sidebar_items(modules, tasks, extras, config) ++
        generate_api_reference(modules, tasks, config) ++
        generate_extras(extras, config) ++
        Formatter.copy_favicon(config, Path.join(@assets_dir, "favicon")) ++
        Formatter.copy_logo(config, Path.join(@assets_dir, "logo")) ++
        generate_search(config) ++
        generate_not_found(config) ++
        generate_list(modules, config) ++
        generate_list(tasks, config) ++
        generate_redirects(config, ".html")

    entrypoint = config.output |> Path.join("index.html") |> Path.relative_to_cwd()
    %{entrypoint: entrypoint, build: all_files}
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

  defp generate_sidebar_items(modules, tasks, extras, config) do
    content = Templates.create_sidebar_items(config, modules, tasks, extras)

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
          ExDoc.warn("file #{Path.relative_to_cwd(output)} already exists", [])
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

  defp additional_assets(config) do
    [
      {Assets.dist(config.proglang), "dist"},
      {Assets.fonts(), "dist"}
    ]
  end

  defp generate_api_reference(_modules, _tasks, %{api_reference: false}) do
    []
  end

  defp generate_api_reference(modules, tasks, config) do
    filename = "api-reference.html"
    output = "#{config.output}/#{filename}"
    config = set_canonical_url(config, filename)

    html = Templates.api_reference_template(config, modules, tasks)

    if File.regular?(output) do
      ExDoc.warn("file #{Path.relative_to_cwd(output)} already exists", [])
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

      destination =
        case String.split(to, "#", parts: 2) do
          [to, anchor] -> to <> ext <> "#" <> anchor
          _ -> to <> ext
        end

      generate_redirect(source, config, destination)

      source
    end)
  end

  defp extension_name(input) do
    input
    |> Path.extname()
    |> String.downcase()
  end

  defp generate_redirect(filename, config, redirect_to) do
    without_anchor = String.split(redirect_to, "#") |> hd()

    unless case_sensitive_file_regular?("#{config.output}/#{without_anchor}") do
      ExDoc.warn("#{filename} redirects to #{redirect_to}, which does not exist", [])
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
    content = Templates.module_template(config, module_node)
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
