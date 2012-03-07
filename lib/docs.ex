defmodule ExDoc do
  def get_docs(files) do
    Enum.map files, get_docs_from_file(&1)
  end

  ####

  def generate_markdown([{name,{ moduledoc, docs }}|t]) do
    buffer = generate_markdown_for_moduledoc(moduledoc) ++
      generate_markdown_for_docs(docs, '')

    Erlang.file.write_file('html/' ++ name ++ '.md', list_to_binary(buffer))

    generate_markdown t
  end

  def generate_markdown([]) do
  end

  ####
  # Helpers
  ####

  defp get_docs_from_file(file) do
    module_name = get_module_name(file)
    module = :"#{module_name}"
    moduledoc = module.__info__(:moduledoc)
    docs = module.__info__(:docs)
    { module_name, { moduledoc, docs } }
  end

  defp get_module_name(name) do
    # TODO: Create an Elixir wrapper to this Erlang function
    :filename.basename(name, '.beam')
  end

  defp generate_markdown_for_moduledoc({_line, doc}) do
    to_char_list(doc)
  end

  defp generate_markdown_for_moduledoc(nil) do
    ''
  end

  # TODO: Refactor this method to use Enum.map
  defp generate_markdown_for_docs([h|t], buffer) do
    buffer = buffer ++ extract_docs(h)
    generate_markdown_for_docs(t, buffer)
  end

  defp generate_markdown_for_docs(doc, buffer) when doc == nil \
                                               when doc == [] do
    buffer
  end

  # TODO: Refactor this method to use string interpolation
  defp extract_docs({ { name, arity }, _line, _type, doc }) do
    to_char_list(name) ++ '/' ++ to_char_list(arity) ++ '\n' ++ to_char_list(doc) ++ '\n'
  end
end
