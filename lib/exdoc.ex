defmodule ExDoc do
  def generate_html(files) do
    docs = ExDoc::Retriever.get_docs(files)
    Enum.map docs, write_html_to_file(&1)
  end

  ####
  # Helpers
  ####

  defp write_html_to_file({name,{ moduledoc, docs }}) do
    html = "<h1>#{name}</h1>\n#{generate_html_for_moduledoc(moduledoc)}\n#{generate_html_for_docs(docs)}\n"
    path = File.expand_path("../../output/#{name}.html", __FILE__)

    Erlang.file.write_file(path, html)
  end

  defp generate_html_for_moduledoc({_line, doc}) do
    html = Markdown.to_html(doc)
    "<div id=\"moduledoc\">\n#{html}\n</div>"
  end

  defp generate_html_for_moduledoc(nil) do
    ""
  end

  defp generate_html_for_docs(docs) do
    Enum.map docs, extract_docs(&1)
  end

  defp extract_docs({ { name, arity }, _line, _type, doc }) do
    html = Markdown.to_html(doc)
    "\n<div id=\"#{name}_#{arity}\">\n#{name}/#{arity}\n#{html}\n</div>"
  end
end
