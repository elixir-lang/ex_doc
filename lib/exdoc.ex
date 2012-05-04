defmodule ExDoc do
  require Erlang.file, as: F

  def generate_docs(path, output_path // "output", formatter // ExDoc.HTMLFormatter) do
    nodes = ExDoc.Retriever.get_docs find_files(path), File.expand_path(path)
    copy_index_files output_path
    copy_css_files output_path
    copy_javascript_files output_path
    copy_image_files output_path
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

  defp copy_image_files(output_path) do
    copy_files "*.png", "../templates/i", "#{output_path}/i"
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

  defp generate_search_index(nodes, base_output_path) do
    output_path = File.expand_path "#{base_output_path}/panel"
    F.make_dir output_path
    content = generate_html_from_nodes nodes
    Erlang.file.write_file("#{output_path}/index.html", content)
  end

  defp get_function_name({ { _name, _arity }, _, _, false }) do
    false
  end

  defp get_function_name({ { name, arity }, _, _, _ }) do
    "#{name}/#{arity}"
  end

  defp generate_html_from_nodes(nodes) do
    template_path = File.expand_path "../templates/panel_template.eex", __FILE__

    names = Enum.map nodes, generate_list_items(&1, 0)
    bindings = [names: names]

    EEx.eval_file template_path, bindings
  end

  defp generate_list_items(node, level) do
    functions = Enum.map node.docs, get_function_name(&1)
    children  = Enum.map node.children, generate_list_items(&1, level + 1)
    module    = inspect(node.module)
    functions = functions_list module, functions, level + 1
    "<li class='level_#{level} closed'>\n#{wrap_link module}\n</li>\n#{children}\n#{functions}"
  end

  defp functions_list(module, functions, level) do
    Enum.map functions, function_list_item(module, &1, level)
  end

  defp function_list_item(module, function, level) do
    "<li class='level_#{level} closed'>\n#{wrap_link module, function}\n</li>\n"
  end

  defp wrap_link(module) do
    "<div class='content'>\n#{link_to_file module, nil}\n<div class='icon'></div>\n</div>"
  end

  defp wrap_link(module, function) do
    "<div class='content'>\n#{link_to_file module, function}\n</div>"
  end

  defp link_to_file(module, nil) do
    "<a href='../#{module}.html' target='docwin'>#{module}</a>"
  end

  defp link_to_file(module, function) do
    "<a href='../#{module}.html##{function}' target='docwin'>#{function}</a>"
  end
end
