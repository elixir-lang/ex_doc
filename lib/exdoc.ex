defmodule ExDoc do
  require Erlang.file, as: F

  def generate_docs(path, formatter // ExDoc.HTMLFormatter) do
    docs = ExDoc.Retriever.get_docs find_files(path), File.expand_path(path)
    copy_index_files
    copy_css_files
    copy_javascript_files
    copy_image_files
    generate_seach_index docs
    Enum.map docs, formatter.format_docs(&1)
  end

  ####
  # Helpers
  ####

  defp find_files(path) do
    path = List.reverse binary_to_list(path)
    File.wildcard normalize_path(path)
  end

  defp normalize_path('/' ++ path) do
    List.reverse(path) ++ '**/*.beam'
  end

  defp normalize_path(path) do
    List.reverse(path) ++ '/**/*.beam'
  end

  defp copy_index_files do
    copy_file "../templates", "output", "index.html"
  end

  defp copy_css_files do
    copy_files "*.css", "../templates/css", "output/css"
  end

  defp copy_javascript_files do
    copy_files "*.js", "../templates/js", "output/js"
  end

  defp copy_image_files do
    copy_files "*.png", "../templates/i", "output/i"
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

  defp generate_seach_index(docs) do
    output_path = File.expand_path "output/panel"
    F.make_dir output_path
    names = Enum.map docs, get_names(&1)
    content = generate_html_from_names names
    Erlang.file.write_file("#{output_path}/index.html", content)
  end

  defp get_names({ module_name, { _, functions }}) do
    functions = Enum.map functions, get_function_name(&1)
    { module_name, functions }
  end

  defp get_function_name({ { _name, _arity }, _, _, false }) do
    false
  end

  defp get_function_name({ { name, arity }, _, _, _ }) do
    "#{name}/#{arity}"
  end

  defp generate_html_from_names(names) do
    template_path = File.expand_path "../templates/panel_template.eex", __FILE__

    names = Enum.map names, generate_list_items(&1)
    bindings = [names: names]

    EEx.eval_file template_path, bindings
  end

  defp generate_list_items({ module, functions }) do
    functions = functions_list module, functions
    "<li class='level_0 closed'>\n#{wrap_link module}\n</li>\n#{functions}"
  end

  defp functions_list(module, functions) do
    Enum.map functions, function_list_item(module, &1)
  end

  defp function_list_item(module, function) do
    "<li class='level_1 closed'>\n#{wrap_link module, function}\n</li>\n"
  end

  defp wrap_link(module, function // nil) do
    "<div class='content'>\n#{link_to_file module, function}\n<div class='icon'></div>\n</div>"
  end

  defp link_to_file(module, nil) do
    "<a href='../#{module}.html' target='docwin'>#{module}</a>"
  end

  defp link_to_file(module, function) do
    "<a href='../#{module}.html##{function}' target='docwin'>#{function}</a>"
  end
end
