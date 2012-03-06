defmodule EDocs do
  def get_docs([h|t], buffer) do
    buffer = get_docs_0(h, buffer)
    get_docs(t, buffer)
  end

  def get_docs([], buffer) do
    buffer
  end

  def generate_markdown([{name,{_line, doc }}|t]) do
    Erlang.file.write_file('html/' ++ name ++ '.md', to_binary(doc))
    generate_markdown t
  end

  def generate_markdown([{name, doc}|t]) do
    Erlang.file.write_file('html/' ++ name ++ '.md', to_binary(doc))
    generate_markdown t
  end

  def generate_markdown([]) do
  end

  ####

  defp get_docs_0(file, buffer) do
    name = get_module_name(file)
    moduledoc = list_to_atom(name).__info__(:moduledoc)
    List.append buffer, [{ name, moduledoc }]
  end

  defp get_module_name('for_docs/' ++ name) do
    get_module_name_0(List.reverse(name))
  end

  defp get_module_name_0('maeb.' ++ name) do
    List.reverse(name)
  end
end
