defmodule ExDoc do
  require Erlang.file, as: F

  def generate_docs(path, output_path // "output", formatter // ExDoc.HTMLFormatter) do
    output_path = File.expand_path(output_path)
    F.make_dir output_path

    path  = File.expand_path(path)
    pairs = ExDoc.Retriever.get_docs find_files(path), path

    Enum.each formatter.assets, copy_assets(&1, output_path)

    Enum.each pairs, fn({ name, nodes }) ->
      generate_listing name, nodes, output_path, formatter
    end
  end

  ####
  # Helpers
  ####

  defp generate_each_page([node|t], formatter, output_path) do
    content = formatter.module_page(node)
    F.write_file("#{output_path}/#{node.id}.html", content)

    generate_each_page(node.children, formatter, output_path)
    generate_each_page(t, formatter, output_path)
  end

  defp generate_each_page([], _formatter, _output_path) do
    :ok
  end

  defp find_files(path) do
    File.wildcard :filename.join(path, "**/*.beam")
  end

  defp copy_assets({ pattern, dir }, output_path) do
    output = "#{output_path}/#{dir}"
    F.make_dir output

    Enum.map File.wildcard(pattern), fn(file) ->
      base = File.basename(file)
      F.copy file, "#{output}/#{base}"
    end
  end

  defp generate_listing(scope, nodes, output_path, formatter) do
    generate_each_page(nodes, formatter, output_path)

    output_file = "#{output_path}/#{scope}_list.html"
    F.make_dir output_path

    template_path = File.expand_path "../templates/list_template.eex", __FILE__

    names    = Enum.map nodes, module_list_item(&1)
    bindings = [names: names, scope: scope]

    content = EEx.eval_file template_path, bindings
    Erlang.file.write_file(output_file, content)
  end

  defp module_list_item(node) do
    docs      = Enum.map node.docs, function_list_item(node, &1)
    children  = Enum.map node.children, module_list_item(&1)

    "<li>\n<a class='toggle'></a>\n#{wrap_link node}\n</li>" <>
      "<ul>#{children}\n#{docs}</ul>"
  end

  defp function_list_item(module, function) do
    "<li>\n#{wrap_link module, function}\n</li>\n"
  end

  defp wrap_link(module) do
    "<span class='object_link'><a href='#{module.id}.html'>#{module.relative}</a>" <>
      "<small class='search_info'>#{module.id}</small>"
  end

  defp wrap_link(module, function) do
    "<span class='object_link'><a href='#{module.id}.html##{function.id}'>#{function.id}</a>" <>
      "<small class='search_info'>#{module.id}</small>"
  end
end
