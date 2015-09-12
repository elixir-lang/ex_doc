defmodule ExDoc.Formatter.HTML do
  @moduledoc false

  alias ExDoc.Formatter.HTML.Templates
  alias ExDoc.Formatter.HTML.Autolink

  @main "overview"

  @doc """
  Generate HTML documentation for the given modules
  """
  @spec run(list, %ExDoc.Config{}) :: String.t
  def run(module_nodes, config) when is_map(config) do
    config = normalize_config(config)
    output = Path.expand(config.output)
    File.rm_rf! output
    :ok = File.mkdir_p output

    generate_assets(output, config)

    all = Autolink.all(module_nodes)
    modules    = filter_list(:modules, all)
    exceptions = filter_list(:exceptions, all)
    protocols  = filter_list(:protocols, all)

    if config.logo do
      config = process_logo_metadata(config)
    end

    unless Enum.empty?(config.extras) do
      generate_extras(output, module_nodes, config, modules, exceptions, protocols)
    end

    generate_index(output, config)
    generate_overview(modules, exceptions, protocols, output, config)
    generate_not_found(modules, exceptions, protocols, output, config)
    generate_sidebar_items(modules, exceptions, protocols, output)
    generate_list(modules, all, output, config)
    generate_list(exceptions, all, output, config)
    generate_list(protocols, all, output, config)

    Path.join(config.output, "index.html")
  end

  # Builds `config` by setting default values and checking for non-valid ones.
  @spec normalize_config(%ExDoc.Config{}) :: %ExDoc.Config{}
  defp normalize_config(config) when is_map(config) do
    if config.main == "index" do
      raise ArgumentError, message: "\"main\" cannot be set to \"index\", otherwise it will recursively link to itself"
    end

    Map.put(config, :main, config.main || @main)
  end

  defp generate_index(output, config) do
    generate_redirect(output, "index.html", config, "#{config.main}.html")
  end

  defp generate_overview(modules, exceptions, protocols, output, config) do
    content = Templates.overview_template(config, modules, exceptions, protocols)
    :ok = File.write("#{output}/overview.html", content)
  end

  defp generate_not_found(modules, exceptions, protocols, output, config) do
    content = Templates.not_found_template(config, modules, exceptions, protocols)
    :ok = File.write("#{output}/404.html", content)
  end

  defp generate_sidebar_items(modules, exceptions, protocols, output) do
    input = for node <- [%{id: "modules", value: modules}, %{id: "exceptions", value: exceptions}, %{id: "protocols", value: protocols}], !Enum.empty?(node.value), do: node
    content = Templates.create_sidebar_items(input)
    :ok = File.write("#{output}/dist/sidebar_items.js", content)
  end

  defp assets do
    [{ templates_path("dist/*.{css,js}"), "dist" },
     { templates_path("fonts/*.{eot,svg,ttf,woff,woff2}"), "fonts" }]
  end

  defp generate_assets(output, _config) do
    Enum.each assets, fn({ pattern, dir }) ->
      output = "#{output}/#{dir}"
      File.mkdir output

      Enum.map Path.wildcard(pattern), fn(file) ->
        base = Path.basename(file)
        File.copy file, "#{output}/#{base}"
      end
    end
  end

  defp generate_extras(output, module_nodes, config, modules, exceptions, protocols) do
    config.extras
    |> Enum.map(&Task.async(fn -> generate_extra(&1, output, module_nodes, config, modules, exceptions, protocols) end))
    |> Enum.map(&Task.await/1)
  end

  defp generate_extra(input, output, module_nodes, config, modules, exceptions, protocols) do
    file_path = Path.expand(input)
    file_extname =
      Path.extname(file_path)
      |> String.downcase

    if file_extname == ".md" do
      file_name =
        Path.basename(file_path, ".md")
        |> String.upcase
      content =
        File.read!(file_path)
        |> Autolink.project_doc(module_nodes)

      config = Map.put(config, :title, file_name)
      extra_html = Templates.extra_template(config, modules, exceptions, protocols, content) |> pretty_codeblocks
      File.write!("#{output}/#{file_name}.html", extra_html)
    else
      raise ArgumentError, "file format not recognized, allowed format is: .md"
    end
  end

  defp process_logo_metadata(config) do
    logo_path = Path.expand config.logo
    output = "#{config.output}/assets"
    File.mkdir_p! output
    file_extname = Path.extname(config.logo) |> String.downcase

    if file_extname in ~w(.png .jpg) do
      file_name = "#{output}/logo#{file_extname}"
      File.copy!(logo_path, file_name)
      Map.put(config, :logo, Path.basename(file_name))
    else
      raise ArgumentError, "image format not recognized, allowed formats are: .jpg, .png"
    end
  end

  defp generate_redirect(output, file_name, config, redirect_to) do
    content = Templates.redirect_template(config, redirect_to)
    :ok = File.write("#{output}/#{file_name}", content)
  end

  @doc false
  # Helper to handle plain code blocks (```...```) with and without
  # language specification and indentation code blocks
  def pretty_codeblocks(bin) do
    bin = Regex.replace(~r/<pre><code(\s+class=\"\")?>\s*iex&gt;/,
                        # Add "elixir" class for now, until we have support for
                        # "iex" in highlight.js
                        bin, "<pre><code class=\"iex elixir\">iex&gt;")
    bin = Regex.replace(~r/<pre><code(\s+class=\"\")?>/,
                        bin, "<pre><code class=\"elixir\">")

    bin
  end

  @doc false
  # Helper to split modules into different categories.
  #
  # Public so that code in Template can use it.
  def categorize_modules(nodes) do
    [modules: filter_list(:modules, nodes),
     exceptions: filter_list(:exceptions, nodes),
     protocols: filter_list(:protocols, nodes)]
  end

  def filter_list(:modules, nodes) do
    Enum.filter nodes, &match?(%ExDoc.ModuleNode{type: x} when not x in [:exception, :protocol, :impl], &1)
  end

  def filter_list(:exceptions, nodes) do
    Enum.filter nodes, &match?(%ExDoc.ModuleNode{type: x} when x in [:exception], &1)
  end

  def filter_list(:protocols, nodes) do
    Enum.filter nodes, &match?(%ExDoc.ModuleNode{type: x} when x in [:protocol], &1)
  end

  defp generate_list(nodes, all, output, config) do
    nodes
    |> Enum.map(&Task.async(fn -> generate_module_page(&1, all, output, config) end))
    |> Enum.map(&Task.await/1)
  end

  defp generate_module_page(node, modules, output, config) do
    content = Templates.module_page(node, config, modules)
    File.write("#{output}/#{node.id}.html", content)
  end

  defp templates_path(other) do
    Path.expand("html/templates/#{other}", __DIR__)
  end
end
