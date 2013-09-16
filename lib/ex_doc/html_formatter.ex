defmodule ExDoc.HTMLFormatter do
  @moduledoc """
  Provide HTML-formatted documentation
  """

  defrecord State, config: nil, modules: []

  alias ExDoc.HTMLFormatter.Templates
  alias ExDoc.HTMLFormatter.Autolink

  @doc """
  Generate HTML documentation for the given modules
  """
  def run(modules, config)  do
    state = State[config: config, modules: Enum.map(modules, &(&1.module))]

    output = Path.expand(state.config.output)
    File.mkdir_p output

    generate_index(output, state)
    generate_assets(output, state)
    has_readme = state.config.readme && generate_readme(output)

    modules = Autolink.all(modules)
    
    generate_overview(modules, output, state)

    Enum.each [:modules, :records, :protocols], fn(mod_type) ->
      generate_list(mod_type, modules, output, state, has_readme)
    end
  end

  defp generate_index(output, state) do
    content = Templates.index_template(state)
    File.write("#{output}/index.html", content)
  end
  
  defp generate_overview(modules, output, state) do
    content = Templates.overview_template(
      state,
      filter_list(:modules, modules),
      filter_list(:records, modules),
      filter_list(:protocols, modules)
    )
    File.write("#{output}/overview.html", content)
  end

  defp assets do
    [ { templates_path("css/*.css"), "css" },
      { templates_path("js/*.js"), "js" } ]
  end

  defp generate_assets(output, _state) do
    Enum.each assets, fn({ pattern, dir }) ->
      output = "#{output}/#{dir}"
      File.mkdir output

      Enum.map Path.wildcard(pattern), fn(file) ->
        base = Path.basename(file)
        File.copy file, "#{output}/#{base}"
      end
    end
  end

  defp generate_readme(output) do
    File.rm("#{output}/README.html")
    write_readme(output, File.read("README.md"))
  end

  defp write_readme(output, {:ok, content}) do
    readme_html = Templates.readme_template(content)
    # Allow using nice codeblock syntax for readme too.
    readme_html = String.replace(readme_html, "<pre><code>",
                                 "<pre class=\"codeblock\"><code>")
    File.write("#{output}/README.html", readme_html)
    true
  end

  defp write_readme(_, _) do
    false
  end

  defp filter_list(:records, nodes) do
    Enum.filter nodes, &match?(ExDoc.ModuleNode[type: x] when x in [:record, :exception], &1)
  end

  defp filter_list(:modules, nodes) do
    Enum.filter nodes, &match?(ExDoc.ModuleNode[type: x] when x in [nil, :behaviour], &1)
  end

  defp filter_list(:protocols, nodes) do
    Enum.filter nodes, &match?(ExDoc.ModuleNode[type: x] when x in [:protocol], &1)
  end

  defp generate_list(scope, all, output, state, has_readme) do
    nodes = filter_list(scope, all)
    Enum.each nodes, &generate_module_page(&1, output, state)
    content = Templates.list_page(scope, nodes, state, has_readme)
    File.write("#{output}/#{scope}_list.html", content)
  end

  defp generate_module_page(node, output, state) do
    content = Templates.module_page(node, state)
    File.write("#{output}/#{node.id}.html", content)
  end

  defp templates_path(other) do
    Path.expand("html_formatter/templates/#{other}", __DIR__)
  end
end
