defmodule ExDoc do
  require Erlang.file, as: F

  def generate_html(files) do
    docs = ExDoc::Retriever.get_docs(files)
    move_css_files
    Enum.map docs, write_html_to_file(&1)
  end

  ####
  # Helpers
  ####

  defp move_css_files() do
    css_source_path = File.expand_path("../templates/css/main.css", __FILE__)
    css_path = File.expand_path("../../output/css", __FILE__)
    F.make_dir(css_path)
    F.copy(css_source_path, css_path <> "/main.css")
  end

  defp write_html_to_file({name,{ moduledoc, docs }}) do
    function_docs = generate_html_for_docs(docs)
    moduledoc = generate_html_for_moduledoc(moduledoc)
    path = File.expand_path("../../output/#{name}.html", __FILE__)

    compiled = EEx.file(File.expand_path("../templates/module_template.eex", __FILE__))
    bindings = [name: name, moduledoc: moduledoc, function_docs: function_docs]
    { result, _ } = Code.eval_quoted(compiled, bindings, __FILE__, __LINE__)

    F.write_file(path, result)
  end

  defp generate_html_for_moduledoc({_line, doc}) do
    Markdown.to_html(doc)
  end

  defp generate_html_for_moduledoc(nil) do
    ""
  end

  defp generate_html_for_docs(docs) do
    Enum.map docs, extract_docs(&1)
  end

  defp extract_docs({ { name, arity }, _line, _type, doc }) do
    html = Markdown.to_html(doc)
    "<div class=\"function\"><div class=\"function-title\" id=\"#{name}_#{arity}\">\n<b>#{name}/#{arity}</b>\n</div>\n<div class=\"description\">\n#{html}\n</div>\n</div>\n"
  end
end
