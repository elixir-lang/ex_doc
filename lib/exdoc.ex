defmodule ExDoc do
  require Erlang.file, as: F

  def generate_docs(path, output_path // "output", formatter // ExDoc.HTMLFormatter) do
    nodes = ExDoc.Retriever.get_docs find_files(path), File.expand_path(path)
    copy_index_files output_path
    copy_css_files output_path
    copy_javascript_files output_path
    generate_search_index nodes, output_path
    generate_each_page nodes, formatter, output_path
  end

  ####
  # Helpers
  ####

  defp generate_each_page([node|t], formatter, output_path) do
    name    = inspect(node.module)
    content = formatter.module_page(node)

    F.write_file("#{output_path}/#{name}.html", content)

    generate_each_page(node.children, formatter, output_path)
    generate_each_page(t, formatter, output_path)
  end

  defp generate_each_page([], _formatter, _output_path) do
    :ok
  end

  defp find_files(path) do
    File.wildcard :filename.join(path, "**/*.beam")
  end

  defp copy_index_files(output_path) do
    copy_file "../templates", output_path, "index.html"
  end

  defp copy_css_files(output_path) do
    copy_files "*.css", "../templates/css", "#{output_path}/css"
  end

  defp copy_javascript_files(output_path) do
    copy_files "*.js", "../templates/js", "#{output_path}/js"
  end

  defp copy_files(wildcard, input_path, output_path) do
    files = Enum.map File.wildcard(File.expand_path("#{input_path}/#{wildcard}", __FILE__)),
      File.basename(&1)
    Enum.map files, copy_file(input_path, output_path, &1)
  end

  defp copy_file(input_path, output_path, file) do
    input_path = File.expand_path input_path, __FILE__
    output_path = File.expand_path output_path

    F.make_dir output_path

    F.copy "#{input_path}/#{file}", "#{output_path}/#{file}"
  end

  defp generate_search_index(nodes, output_path) do
    output_file = File.expand_path "#{output_path}/module_list.html"
    F.make_dir output_path
    content = generate_html_from_nodes nodes
    Erlang.file.write_file(output_file, content)
  end

  defp get_function_name({ { _name, _arity }, _, _, false }) do
    false
  end

  defp get_function_name({ { name, arity }, _, _, _ }) do
    "#{name}/#{arity}"
  end

  defp generate_html_from_nodes(nodes) do
    template_path = File.expand_path "../templates/list_template.eex", __FILE__

    names = Enum.map nodes, module_list_item(&1)
    bindings = [names: names]

    EEx.eval_file template_path, bindings
  end

  defp module_list_item(node) do
    functions = Enum.map node.docs, get_function_name(&1)
    functions = Enum.filter functions, fn(x) -> x end
    functions = Enum.map functions, function_list_item(node, &1)

    children  = Enum.map node.children, module_list_item(&1)
    
    "<li>\n<a class='toggle'></a>\n#{wrap_link node}\n</li>" <>
      "<ul>#{children}\n#{functions}</ul>"
  end

  defp function_list_item(module, function) do
    "<li>\n#{wrap_link module, function}\n</li>\n"
  end

  defp wrap_link(node) do
    name = inspect(node.module)
    "<span class='object_link'><a href='#{name}.html'>#{node.relative}</a>" <>
      "<small class='search_info'>#{name}</small>"
  end

  defp wrap_link(node, function) do
    name = inspect(node.module)
    "<span class='object_link'><a href='#{name}.html##{function}'>#{function}</a>" <>
      "<small class='search_info'>#{name}</small>"
  end
end
