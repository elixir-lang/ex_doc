defmodule ExDoc.Formatter.HTML do
  @moduledoc """
  Generate HTML documentation for Elixir projects
  """

  alias ExDoc.Formatter.HTML.Templates
  alias ExDoc.Formatter.HTML.Autolink
  alias ExDoc.Markdown

  @main "api-reference"

  @doc """
  Generate HTML documentation for the given modules
  """
  @spec run(list, ExDoc.Config.t) :: String.t
  def run(project_nodes, config) when is_map(config) do
    config = normalize_config(config)
    output = Path.expand(config.output)
    build = Path.join(output, ".build")
    output_setup(build, output)

    linked = Autolink.all(project_nodes, ".html", config.deps)
    nodes_map = %{
      modules: filter_list(:modules, linked),
      exceptions: filter_list(:exceptions, linked),
      protocols: filter_list(:protocols, linked)
    }

    extras = build_extras(project_nodes, nodes_map, config)
    static_files = generate_assets(output, assets(config))

    generated_files =
      generate_sidebar_items(nodes_map, extras, output) ++
      generate_extras(nodes_map, extras, output, config) ++
      generate_logo("assets", config) ++
      generate_index(output, config) ++
      generate_not_found(nodes_map, output, config) ++
      generate_list(nodes_map.modules, nodes_map, output, config) ++
      generate_list(nodes_map.exceptions, nodes_map, output, config) ++
      generate_list(nodes_map.protocols, nodes_map, output, config)

    generate_build(extras, static_files ++ generated_files, build)
    Path.join(config.output, "index.html")
  end

  defp normalize_config(%{main: "index"}) do
    raise ArgumentError, message: ~S("main" cannot be set to "index", otherwise it will recursively link to itself)
  end
  defp normalize_config(%{main: main} = config) do
    %{config | main: main || @main}
  end

  defp output_setup(build, output) do
    if File.exists? build do
      Enum.each(File.stream!(build), fn(line) ->
        line = String.replace(line, "\n", "")
        File.rm(Path.join(output, line))
      end)
      File.rm(build)
    else
      File.rm_rf! output
      File.mkdir_p! output
    end
  end

  defp generate_build(extras, generated_files, output) do
    extras = for {file, _, _, _} <- extras, do: "#{file}.html"
    build_files = Enum.map(Enum.uniq(generated_files ++ extras), &[&1, "\n"])

    File.write!(output, build_files)
  end

  defp generate_index(output, config) do
    index_file = "index.html"
    main_file = "#{config.main}.html"
    generate_redirect(output, index_file, config, main_file)
    [index_file, main_file]
  end

  defp generate_not_found(nodes_map, output, config) do
    filename = "404.html"
    config = set_canonical_url(config, filename)
    content = Templates.not_found_template(config, nodes_map)
    File.write!("#{output}/#{filename}", content)
    [filename]
  end

  defp generate_sidebar_items(nodes_map, extras, output) do
    content = Templates.create_sidebar_items(nodes_map, extras)

    digest =
      content
      |> :erlang.md5
      |> Base.encode16(case: :lower)
      |> binary_part(0, 10)

    sidebar_items = "dist/sidebar_items-#{digest}.js"
    File.write!(Path.join(output, sidebar_items), content)
    [sidebar_items]
  end

  defp generate_extras(nodes_map, extras, output, config) do
    Enum.map(extras, fn {filename, title, _group, content} ->
      filename = "#{filename}.html"
      config = set_canonical_url(config, filename)
      html = Templates.extra_template(config, title, nodes_map, content)
      if File.regular? output do
        IO.puts "warning: file #{Path.relative_to_cwd output} already exists"
      end
      File.write!("#{output}/#{filename}", html)
      filename
    end)
  end

  @doc """
  Copy a list of assets into a given directory
  """
  @spec generate_assets(list, String.t) :: :ok
  def generate_assets(output, sources) do
    Enum.flat_map sources, fn {from, to} ->
      output = "#{output}/#{to}"

      Enum.map Path.wildcard(Path.join(from, "**/*")), fn source ->
        target = Path.join(output, Path.relative_to(source, from))
        File.mkdir(Path.dirname(target))
        File.copy(source, target)
        target
      end
    end
  end

  defp assets(%{assets: nil}), do: assets()
  defp assets(%{assets: path}), do: [{path, "assets"} | assets()]

  defp assets_path(pattern) do
    Application.app_dir(:ex_doc, "priv/ex_doc/formatter/html/templates/#{pattern}")
  end

  defp assets do
    [{assets_path("dist"), "dist"},
     {assets_path("fonts"), "fonts"}]
  end

  defp build_extras(project_nodes, nodes_map, config) do
    extras =
      config.extras
      |> Enum.map(&Task.async(fn ->
          build_extra(&1, project_nodes)
         end))
      |> Enum.map(&Task.await(&1, :infinity))

    api_reference = Templates.api_reference_template(config, nodes_map)
    [{"api-reference", "API Reference", "", api_reference}|extras]
  end

  defp build_extra({input, options}, project_nodes) do
    input = to_string(input)
    filename = options[:filename] || input |> input_to_title() |> title_to_filename()
    build_extra(input, filename, options[:title], options[:group], project_nodes)
  end

  defp build_extra(input, project_nodes) do
    filename = input |> input_to_title |> title_to_filename
    build_extra(input, filename, nil, "", project_nodes)
  end

  defp build_extra(input, filename, title, group, project_nodes) do
    if valid_extension_name?(input) do
      content =
        input
        |> File.read!()
        |> Autolink.project_doc(project_nodes)

      html_content = Markdown.to_html(content, file: input, line: 1)
      title = title || extract_title(html_content) || input_to_title(input)
      {filename, title, group, html_content}
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

  @doc """
  Extract title from h1 header in the content
  """
  def extract_title(content) do
    title = Regex.run(@h1_regex, content, capture: :all_but_first)

    if title do
      title |> List.first() |> strip_html() |> String.strip()
    end
  end

  @doc """
  Convert the input file name into a title
  """
  def input_to_title(input) do
    input |> Path.basename() |> Path.rootname()
  end

  @doc """
  Convert a title into a file name
  """
  def title_to_filename(title) do
    title |> String.replace(" ", "-") |> String.downcase()
  end

  @doc """
  Generates the logo from config into the given directory
  and adjusts the logo config key.
  """
  def generate_logo(_output, %{logo: nil}) do
    []
  end
  def generate_logo(output, %{output: base, logo: logo}) do
    output = Path.join(base, output)
    File.mkdir_p!(output)

    extname =
      logo
      |> Path.extname()
      |> String.downcase()

    if extname in ~w(.png .jpg) do
      filename = "#{output}/logo#{extname}"
      File.copy!(logo, filename)
      [filename]
    else
      raise ArgumentError, "image format not recognized, allowed formats are: .jpg, .png"
    end
  end

  defp generate_redirect(output, filename, config, redirect_to) do
    content = Templates.redirect_template(config, redirect_to)
    File.write!("#{output}/#{filename}", content)
  end

  def filter_list(:modules, nodes) do
    Enum.filter nodes, &(not &1.type in [:exception, :protocol, :impl])
  end

  def filter_list(:exceptions, nodes) do
    Enum.filter nodes, &(&1.type in [:exception])
  end

  def filter_list(:protocols, nodes) do
    Enum.filter nodes, &(&1.type in [:protocol])
  end

  defp generate_list(nodes, nodes_map, output, config) do
    nodes
    |> Enum.map(&Task.async(fn ->
        generate_module_page(&1, nodes_map, output, config)
       end))
    |> Enum.map(&Task.await(&1, :infinity))
  end

  defp generate_module_page(module_node, nodes_map, output, config) do
    filename = "#{module_node.id}.html"
    config = set_canonical_url(config, filename)
    content = Templates.module_page(module_node, nodes_map, config)
    File.write!("#{output}/#{filename}", content)
    filename
  end

  defp set_canonical_url(config, filename) do
    if config.canonical do
      canonical_url =
        config.canonical
        |> String.rstrip(?/)
        |> Path.join(filename)

      Map.put(config, :canonical, canonical_url)
    else
      config
    end
  end
end
