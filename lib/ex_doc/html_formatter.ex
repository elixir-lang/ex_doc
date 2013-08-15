defmodule ExDoc.HTMLFormatter do
  @moduledoc """
  Provide HTML-formatted documentation
  """

  alias ExDoc.HTMLFormatter.Templates

  @doc """
  Generate HTML documentation for the given modules
  """
  def run(modules, config)  do
    output = Path.expand(config.output)
    File.mkdir_p output

    generate_index(output, config)
    generate_assets(output, config)
    has_readme = config.readme && generate_readme(output)

    Enum.each [:modules, :records, :protocols], fn(mod_type) ->
      modules
        |> ExDoc.Retriever.filter_modules(mod_type)
        |> ExDoc.Retriever.nest_modules(config)
        |> generate_list(mod_type, output, config, has_readme)
    end
  end

  defp generate_index(output, config) do
    content = Templates.index_template(config)
    File.write("#{output}/index.html", content)
  end

  defp assets do
    [ { Templates.templates_path("css/*.css"), "css" },
      { Templates.templates_path("js/*.js"), "js" } ]
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

  defp generate_readme(output) do
    File.rm("#{output}/README.html")
    write_readme(output, File.read("README.md"))
  end

  defp write_readme(output, {:ok, content}) do
    readme_html = Templates.readme_template(content)
    File.write("#{output}/README.html", readme_html)
    true
  end

  defp write_readme(_, _) do
    false
  end

  defp generate_list(nodes, scope, output, config, has_readme) do
    generate_module_page(nodes, output)
    content = Templates.list_template(scope, nodes, config, has_readme)
    File.write("#{output}/#{scope}_list.html", content)
  end

  defp generate_module_page([node|t], output) do
    content = Templates.module_page(node)
    File.write("#{output}/#{node.id}.html", content)

    generate_module_page(node.children, output)
    generate_module_page(t, output)
  end

  defp generate_module_page([], _output) do
    :ok
  end

end
