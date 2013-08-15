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
    generate_module_page(nodes, output, config)
    content = Templates.list_template(scope, nodes, config, has_readme)
    File.write("#{output}/#{scope}_list.html", content)
  end

  defp generate_module_page([node|t], output, config) do
    content = module_page(node, config)
    File.write("#{output}/#{node.id}.html", content)

    generate_module_page(node.children, output, config)
    generate_module_page(t, output, config)
  end

  defp generate_module_page([], _output, _config) do
    :ok
  end

  def module_page(node, _config) do
    functions = Enum.filter node.docs, &match?(ExDoc.FunctionNode[type: :def], &1)
    macros    = Enum.filter node.docs, &match?(ExDoc.FunctionNode[type: :defmacro], &1)
    callbacks = Enum.filter node.docs, &match?(ExDoc.FunctionNode[type: :defcallback], &1)
    fields    = get_fields(node)
    impls     = get_impls(node)

    Templates.module_template(node, functions, macros, callbacks, fields, impls)
  end

  # Get only fields that do not start with underscore
  defp get_fields(ExDoc.ModuleNode[type: type] = node) when type in [:record, :exception] do
    Enum.filter node.module.__record__(:fields), fn({f,_}) ->
      hd(atom_to_list(f)) != ?_
    end
  end

  defp get_fields(_), do: []

  # Loop through all children finding implementations
  defp get_impls(ExDoc.ModuleNode[type: :protocol, module: module, children: children]) do
    Enum.filter children, fn(child) ->
      child.type == :impl && child.module.__impl__(:protocol) == module
    end
  end

  defp get_impls(_), do: []
end
