defmodule ExDoc.Formatter.HTML do
  @moduledoc """
  Provide HTML-formatted documentation
  """

  alias ExDoc.Formatter.HTML.Templates
  alias ExDoc.Formatter.HTML.Autolink

  @doc """
  Generate HTML documentation for the given modules
  """
  def run(module_nodes, config)  do
    output = Path.expand(config.output)
    File.rm_rf! output
    :ok = File.mkdir_p output

    generate_assets(output, config)

    all = Autolink.all(module_nodes)
    modules    = filter_list(:modules, all)
    exceptions = filter_list(:exceptions, all)
    protocols  = filter_list(:protocols, all)

    has_readme = config.readme && generate_readme(output, module_nodes, config, modules, exceptions, protocols)
    generate_overview(modules, exceptions, protocols, output, config, has_readme, config.main)
    generate_sidebar_items(modules, exceptions, protocols, output)
    generate_list(modules, all, output, config, has_readme)
    generate_list(exceptions, all, output, config, has_readme)
    generate_list(protocols, all, output, config, has_readme)

    if config.main do
      generate_main(output, config)
    end

    Path.join(config.output, "index.html")
  end

  defp generate_overview(modules, exceptions, protocols, output, config, has_readme, has_main) do
    content = Templates.overview_template(config, modules, exceptions, protocols, has_readme, has_main)

    if has_readme || has_main do
      :ok = File.write("#{output}/overview.html", content)
    else
      :ok = File.write("#{output}/index.html", content)
    end
  end

  defp generate_sidebar_items(modules, exceptions, protocols, output) do
    input = for node <- [%{id: "modules", value: modules}, %{id: "exceptions", value: exceptions}, %{id: "protocols", value: protocols}], !Enum.empty?(node.value), do: node
    content = Templates.sidebar_items_template(input)
    :ok = File.write("#{output}/sidebar_items.js", content)
  end

  defp assets do
    [{ templates_path("css/*.css"), "css" },
     { templates_path("js/*.js"), "js" },
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

  defp generate_main(output, config) do
    File.cp!("#{output}/#{config.main}.html", "#{output}/index.html")
    true
  end

  defp generate_readme(output, module_nodes, config, modules, exceptions, protocols) do
    readme_path = Path.expand(config.readme)
    write_readme(output, File.read(readme_path), module_nodes, config, modules, exceptions, protocols)
  end

  defp write_readme(output, {:ok, content}, module_nodes, config, modules, exceptions, protocols) do
    content = Autolink.project_doc(content, module_nodes)
    readme_html = Templates.readme_template(config, modules, exceptions, protocols, content) |> pretty_codeblocks

    if config.main do
      File.write("#{output}/readme.html", readme_html)
    else
      File.write("#{output}/index.html", readme_html)
    end

    true
  end

  defp write_readme(_, _, _, _, _, _, _) do
    false
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
