defmodule ExDoc do
  require Erlang.file, as: F

  def generate_docs(path, formatter // ExDoc::HTMLFormatter) do
    docs = ExDoc::Retriever.get_docs find_files(path)
    move_index_files
    move_css_files
    generate_seach_index(docs)
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
    List.reverse(path) ++ '*'
  end

  defp normalize_path(path) do
    List.reverse(path) ++ '/*'
  end

  defp move_css_files do
    Enum.map ["main.css", "reset.css", "panel.css"],
      move_file("../templates/css", "../../output/css", &1)
  end

  defp move_index_files do
    move_file("../templates", "../../output", "index.html")
  end

  defp move_file(input_path, output_path, file) do
    input_path = File.expand_path(input_path, __FILE__)
    output_path = File.expand_path(output_path, __FILE__)

    F.make_dir(output_path)

    filename = "/#{file}"

    F.copy(input_path <> filename, output_path <> filename)
  end

  defp generate_seach_index(docs) do
    output_path = File.expand_path("../../output/panel", __FILE__)
    F.make_dir(output_path)
    names = Enum.map docs, get_names(&1)
    content = generate_html_from_names(names)
    Erlang.file.write_file(output_path <> "/index.html", content)
  end

  defp get_names({ module_name, { _, functions }}) do
    functions = Enum.map functions, get_function_name(&1)
    { module_name, functions }
  end

  defp get_function_name({ { name, arity }, _, _, _ }) do
    "#{name}/#{arity}"
  end

  defp generate_html_from_names(names) do
    template_path = File.expand_path("../templates/panel_template.eex", __FILE__)

    names = Enum.map names, generate_list_items(&1)
    bindings = [names: names]

    EEx.eval_file(template_path, bindings)
  end

  defp generate_list_items({ module, functions }) do
    functions = functions_list(module, functions)
    "<li>#{link_to_file(module)}#{functions}</li>\n"
  end

  defp functions_list(module, functions) do
    function_items = Enum.map functions, function_list_item(module, &1)
    unless Enum.empty?(function_items) do
      functions_ul = "\n<ul>\n#{function_items}</ul>\n"
    end
    functions_ul
  end

  defp function_list_item(module, function) do
    "<li>#{link_to_file(module, function)}</li>\n"
  end

  defp link_to_file(module) do
    "<a href='../#{module}.html' target='docwin'>#{module}</a>"
  end

  defp link_to_file(module, function) do
    "<a href='../#{module}.html##{function}' target='docwin'>#{function}</a>"
  end
end
