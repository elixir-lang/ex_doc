defmodule ExDoc.Formatter.HTML do
  @moduledoc """
  Provide HTML-formatted documentation
  """

  alias ExDoc.Formatter.HTML.Templates
  alias ExDoc.Formatter.HTML.Autolink

  # default values
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

    has_readme = config.readme && generate_readme(output, module_nodes, config, modules, exceptions, protocols)
    generate_index(output, config)
    generate_overview(modules, exceptions, protocols, output, config, has_readme)
    generate_sidebar_items(modules, exceptions, protocols, output)
    generate_list(modules, all, output, config, has_readme)
    generate_list(exceptions, all, output, config, has_readme)
    generate_list(protocols, all, output, config, has_readme)

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

  defp generate_overview(modules, exceptions, protocols, output, config, has_readme) do
    content = Templates.overview_template(config, modules, exceptions, protocols, has_readme)
    :ok = File.write("#{output}/overview.html", content)
  end

  defp generate_sidebar_items(modules, exceptions, protocols, output) do
    input = for node <- [%{id: "modules", value: modules}, %{id: "exceptions", value: exceptions}, %{id: "protocols", value: protocols}], !Enum.empty?(node.value), do: node
    content = Templates.sidebar_items_template(input)
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

  defp generate_readme(output, module_nodes, config, modules, exceptions, protocols) do
    readme_path = Path.expand(config.readme)
    write_readme(output, File.read(readme_path), module_nodes, config, modules, exceptions, protocols)
  end

  defp write_readme(output, {:ok, content}, module_nodes, config, modules, exceptions, protocols) do
    content = Autolink.project_doc(content, module_nodes)
    readme_html = Templates.readme_template(config, modules, exceptions, protocols, content) |> pretty_codeblocks
    :ok = File.write("#{output}/README.html", readme_html)
    true
  end

  defp write_readme(_, _, _, _, _, _, _) do
    false
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

  defp generate_list(nodes, all, output, config, has_readme) do
    nodes
    |> Enum.map(&Task.async(fn -> generate_module_page(&1, all, output, config, has_readme) end))
    |> Enum.map(&Task.await/1)
  end

  defp generate_module_page(node, modules, output, config, has_readme) do
    content = Templates.module_page(node, config, modules, has_readme)
    File.write("#{output}/#{node.id}.html", content)
  end

  defp templates_path(other) do
    Path.expand("html/templates/#{other}", __DIR__)
  end
end
