defmodule ExDoc do
  require Erlang.file, as: F

  def generate_docs(path, formatter // ExDoc::HTMLFormatter) do
    docs = ExDoc::Retriever.get_docs find_files(path)
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

  defp move_css_files do
    css_path = File.expand_path("../../output/css", __FILE__)
    template_path = File.expand_path("../templates/css/main.css", __FILE__)

    F.make_dir(css_path)

    F.copy(template_path, css_path <> "/main.css")
  end

  defp generate_seach_index(docs) do
    output_path = File.expand_path("../../output", __FILE__)
    names = Enum.map docs, get_names(&1)
    content = generate_html_from_names(names)
    Erlang.file.write_file(output_path <> "/index.html", content)
  end

  defp get_names({ module_name, { _, functions }}) do
    functions = Enum.map functions, get_function_name(&1)
    { module_name, functions }
  end

  defp generate_html_from_names(names) do
    template_path = File.expand_path("../templates/index_template.eex", __FILE__)

    names = Enum.map names, generate_list_items(&1)
    bindings = [names: names]

    EEx.eval_file(template_path, bindings)
  end

  defp generate_list_items({ module, functions }) do
    functions = functions_list(functions)
    "<li>#{module}#{functions}</li>\n"
  end

  defp get_function_name({ { function, arity }, _, _, _ }) do
    "#{function}/#{arity}"
  end

  defp functions_list(functions) do
    function_items = Enum.map functions, function_list_item(&1)
    unless Enum.empty?(function_items) do
      functions_ul = "\n<ul>\n#{function_items}</ul>\n"
    end
    functions_ul
  end

  defp function_list_item(function) do
    "<li>#{function}</li>\n"
  end

  defp normalize_path('/' ++ path) do
    List.reverse(path) ++ '*'
  end

  defp normalize_path(path) do
    List.reverse(path) ++ '/*'
  end
end
