defmodule ExDoc::HTMLFormatter do
  def format_docs({name,{ moduledoc, docs }}) do
    docs = generate_html_for_docs(docs)
    moduledoc = generate_html_for_moduledoc(moduledoc)
    function_docs = Enum.filter_map docs, fn(x, do: filter_by_type(x, :def)), fn(x, do: get_content(x))
    macro_docs = Enum.filter_map docs, fn(x, do: filter_by_type(x, :defmacro)), fn(x, do: get_content(x))

    bindings = [name: name, moduledoc: moduledoc, function_docs: function_docs, macro_docs: macro_docs]
    content = compile_template(bindings)

    Erlang.file.write_file(output_path <> "/#{name}.html", content)
  end

  defp compile_template(bindings) do
    compiled = EEx.file(template_path <> "/module_template.eex")
    { content, _ } = Code.eval_quoted(compiled, bindings, __FILE__, __LINE__)
    content
  end

  def filter_by_type(function, expected) do
    { type, _ } = function
    type == expected
  end

  def get_content(function) do
    { _, content } = function
    content
  end

  defp generate_html_for_moduledoc({_, nil}) do
    nil
  end

  defp generate_html_for_moduledoc({_line, doc}) do
    Markdown.to_html(doc)
  end

  defp generate_html_for_docs(docs) do
    Enum.map docs, extract_docs(&1)
  end

  defp extract_docs({ { name, arity }, _line, type, doc }) do
    html = Markdown.to_html(doc)
    content = "<div class=\"function\"><div class=\"function-title\" id=\"#{name}_#{arity}\">\n<b>#{name}/#{arity}</b>\n</div>\n<div class=\"description\">\n#{html}\n</div>\n</div>\n"
    { type, content }
  end

  defp template_path() do
    File.expand_path("../../templates", __FILE__)
  end

  defp output_path() do
    File.expand_path("../../../output", __FILE__)
  end
end
