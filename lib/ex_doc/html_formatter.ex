defmodule ExDoc.HTMLFormatter do
  @moduledoc """
  Provide HTML-formatted documentation
  """

  alias ExDoc.HTMLFormatter.Templates
  alias ExDoc.HTMLFormatter.Autolink

  @doc """
  Generate HTML documentation for the given modules
  """
  def run(modules, config)  do
    output = Path.expand(config.output)
    File.mkdir_p output

    generate_index(output, config)
    generate_assets(output, config)
    has_readme = config.readme && generate_readme(output)

    modules = Autolink.all(modules)

    Enum.each [:modules, :records, :protocols], fn(mod_type) ->
      generate_list(mod_type, modules, output, config, has_readme)
    end
  end

  @doc """
  Generates the project index in `output`.
  """
  def generate_project_index(output) do
    output = Path.expand(output)
    File.mkdir_p output

    generate_assets(output, nil) # Doesn't use config arg actually, so nil is safe
    
    projects = scan_projects(output)
    content = Templates.project_index_template(projects)
    File.write("#{output}/index.html", content)
  end

  defp generate_index(output, config) do
    content = Templates.index_template(config)
    File.write("#{output}/index.html", content)
  end

  defp assets do
    [ { templates_path("css/*.css"), "css" },
      { templates_path("js/*.js"), "js" } ]
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

  defp filter_list(:records, nodes) do
    Enum.filter nodes, &match?(ExDoc.ModuleNode[type: x] when x in [:record, :exception], &1)
  end

  defp filter_list(:modules, nodes) do
    Enum.filter nodes, &match?(ExDoc.ModuleNode[type: x] when x in [nil, :behaviour], &1)
  end

  defp filter_list(:protocols, nodes) do
    Enum.filter nodes, &match?(ExDoc.ModuleNode[type: x] when x in [:protocol], &1)
  end

  defp generate_list(scope, all, output, config, has_readme) do
    nodes = filter_list(scope, all)
    Enum.each nodes, &generate_module_page(&1, output)
    content = Templates.list_page(scope, nodes, config, has_readme)
    File.write("#{output}/#{scope}_list.html", content)
  end

  defp generate_module_page(node, output) do
    content = Templates.module_page(node)
    File.write("#{output}/#{node.id}.html", content)
  end

  defp templates_path(other) do
    Path.expand("html_formatter/templates/#{other}", __DIR__)
  end

  defp scan_projects(output) do
    Path.wildcard(Path.join(output, "**/index.html"))
    |> Stream.map(&extract_summary(&1, output))
    |> Stream.reject(&(&1 == nil))
    |> Enum.sort(fn (a, b) -> 
        # The order of fields in Mix.Schema is just so that comparison
        # works.
        { String.downcase(a[:name]), Mix.Version.parse(a[:version]) } < 
        { String.downcase(b[:name]), Mix.Version.parse(b[:version]) }
       end)
  end

  defp extract_summary(path, output) do
    case File.read(path) do
      { :error, _ }     -> nil
      { :ok, contents } ->
        summary =
          String.split(contents, %r/\r?\n/)
          |> Stream.drop_while(&!String.starts_with?(&1, "EX_DOC_SUMMARY_START"))
          |> Stream.drop(1)
          |> Stream.take_while(&!String.starts_with?(&1, "EX_DOC_SUMMARY_END"))
          |> Stream.map(&summary_line_to_tup/1)
          |> Enum.reject(&(&1 == nil))
        cond do
          summary[:name]    == nil -> nil
          summary[:version] == nil -> nil
          true                     -> [path: Path.relative_to(path, output)] ++ summary
        end
    end
  end

  defp summary_line_to_tup(line) do
    case String.split(line, ":", global: false) do
      [key, value] -> { binary_to_atom(String.strip(key)), String.strip(value) }
      _            -> nil
    end
  end
end
