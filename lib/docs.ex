defmodule EDocs do
  def get_docs([h|t], buffer) do
    buffer = get_docs_0(h, buffer)
    get_docs(t, buffer)
  end

  def get_docs([], buffer) do
    buffer
  end

  def generate_markdown([{name,{ moduledoc, docs }}|t]) do
    buffer = generate_markdown_for_moduledoc(moduledoc) ++
      generate_markdown_for_docs(docs, '')

    Erlang.file.write_file('html/' ++ name ++ '.md', list_to_binary(buffer))

    generate_markdown t
  end

  def generate_markdown([]) do
  end

  ####

  defp get_docs_0(file, buffer) do
    name = get_module_name(file)
    module = list_to_atom(name)
    moduledoc = module.__info__(:moduledoc)
    docs = module.__info__(:docs)
    List.append buffer, [{ name, { moduledoc, docs } }]
  end

  defp get_module_name('samples/for_docs/' ++ name) do
    get_module_name_0(List.reverse(name))
  end

  defp get_module_name_0('maeb.' ++ name) do
    List.reverse(name)
  end

  defp generate_markdown_for_moduledoc({_line, doc}) do
    to_char_list(doc)
  end

  defp generate_markdown_for_moduledoc(nil) do
    ''
  end

  defp generate_markdown_for_docs([h|t], buffer) do
    buffer = buffer ++ extract_docs(h)
    generate_markdown_for_docs(t, buffer)
  end

  defp generate_markdown_for_docs(doc, buffer) when doc == nil \
                                               when doc == [] do
    buffer
  end

  defp extract_docs({ { name, arity }, _line, _type, doc }) do
    to_char_list(name) ++ '/' ++ to_char_list(arity) ++ '\n' ++ to_char_list(doc) ++ '\n'
  end
end
