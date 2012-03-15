defmodule ExDoc do
  require Erlang.file, as: F

  def generate_docs(files, formatter // ExDoc::HTMLFormatter) do
    docs = ExDoc::Retriever.get_docs(files)
    move_css_files
    generate_seach_index(docs)
    Enum.map docs, formatter.format_docs(&1)
  end

  ####
  # Helpers
  ####

  defp move_css_files() do
    css_path = File.expand_path("../../output/css", __FILE__)
    template_path = File.expand_path("../templates/css/main.css", __FILE__)

    F.make_dir(css_path)

    F.copy(template_path, css_path <> "/main.css")
  end

  defp generate_seach_index(docs) do
    output_path = File.expand_path("../../output", __FILE__)
    modules = Enum.map docs, get_module(&1)
    content = generate_html_from_modules(modules)
    Erlang.file.write_file(output_path <> "/index.html", content)
  end

  defp get_module({name, _}) do
    name
  end

  defp generate_html_from_modules(modules) do
    modules = Enum.map modules, generate_list_item(&1)
    bindings = [modules: modules]
    compile_template(bindings)
  end

  defp compile_template(bindings) do
    template_path = File.expand_path("../templates/index_template.eex", __FILE__)
    compiled = EEx.compile(File.read!(template_path))
    { content, _ } = Code.eval_quoted(compiled, bindings, __FILE__, __LINE__)
    content
  end

  defp generate_list_item(module) do
    "<li>#{module}</li>\n"
  end
end
