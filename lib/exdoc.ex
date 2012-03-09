defmodule ExDoc do
  def get_docs(files) do
    ExDoc::Retriever.get_docs(files)
  end

  ####

  def generate_markdown(docs) do
    Enum.map docs, write_markdown_to_file(&1)
  end

  ####
  # Helpers
  ####

  defp write_markdown_to_file({name,{ moduledoc, docs }}) do
    buffer = "#{generate_markdown_for_moduledoc(moduledoc)}\n#{generate_markdown_for_docs(docs)}"
    path = File.expand_path("../../output/#{name}.md", __FILE__)

    Erlang.file.write_file(path, buffer)
  end

  defp generate_markdown_for_moduledoc({_line, doc}) do
    to_char_list(doc)
  end

  defp generate_markdown_for_moduledoc(nil) do
    ''
  end

  defp generate_markdown_for_docs(docs) do
    Enum.map docs, extract_docs(&1)
  end

  defp extract_docs({ { name, arity }, _line, _type, doc }) do
    "#{name}/#{arity}\n#{doc}\n"
  end
end
