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
    F.make_dir(output_path <> "/css")
    F.copy(template_path <> "/css/main.css", output_path <> "/css/main.css")
  end

  defp write_html_to_file({name,{ moduledoc, docs }}) do
    function_docs = generate_html_for_docs(docs)
    moduledoc = generate_html_for_moduledoc(moduledoc)

    bindings = [name: name, moduledoc: moduledoc, function_docs: function_docs]
    content = compile_template(bindings)

    F.write_file(output_path <> "/#{name}.html", content)
  end

  defp compile_template(bindings) do
    compiled = EEx.file(template_path <> "/module_template.eex")
    { content, _ } = Code.eval_quoted(compiled, bindings, __FILE__, __LINE__)
    content
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

  defp template_path() do
    File.expand_path("../templates", __FILE__)
  end

  defp output_path() do
    File.expand_path("../../output", __FILE__)
  end
end
