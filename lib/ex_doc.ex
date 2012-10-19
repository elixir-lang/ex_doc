defmodule ExDoc do
  def generate_docs(path, options // []) do
    output_path = options[:output]      || "output"
    formatter   = options[:formatter]   || ExDoc.HTMLFormatter
    project_url = options[:project_url] || "https://github.com/elixir-lang/elixir/blob/master/%{path}#L%{line}"

    pairs = ExDoc.Retriever.get_docs find_beams(path), project_url

    output_path = File.expand_path(output_path)
    File.mkdir_p output_path

    Enum.each formatter.assets, copy_assets(&1, output_path)
    Enum.each pairs, fn({ name, nodes }) ->
      generate_list name, nodes, output_path, formatter
    end
  end

  # Helpers

  defp find_beams(path) do
    File.wildcard File.expand_path("Elixir-*.beam", path)
  end

  defp copy_assets({ pattern, dir }, output_path) do
    output = "#{output_path}/#{dir}"
    File.mkdir output

    Enum.map File.wildcard(pattern), fn(file) ->
      base = File.basename(file)
      File.copy file, "#{output}/#{base}"
    end
  end

  defp generate_list(scope, nodes, output_path, formatter) do
    generate_module_page(nodes, formatter, output_path)
    output_file = "#{output_path}/#{scope}_list.html"
    content     = formatter.list_page(scope, nodes)
    File.write(output_file, content)
  end

  defp generate_module_page([node|t], formatter, output_path) do
    content = formatter.module_page(node)
    File.write("#{output_path}/#{node.id}.html", content)

    generate_module_page(node.children, formatter, output_path)
    generate_module_page(t, formatter, output_path)
  end

  defp generate_module_page([], _formatter, _output_path) do
    :ok
  end
end
