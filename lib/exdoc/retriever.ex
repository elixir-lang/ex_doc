defrecord ExDoc.Node, module: nil, relative: nil, moduledoc: nil,
  docs: [], source: nil, children: []

defmodule ExDoc.Retriever do
  def get_docs(files, relative_to) do
    parts   = File.split "#{relative_to}/__MAIN__"
    modules = Enum.map files, get_module_from_file(&1, parts)
    get_docs_from_modules([], Enum.qsort(modules), [])
  end

  # Helpers

  defp get_docs_from_modules(scope, [h|t], acc) do
    flag   = scope ++ h
    length = length(flag)

    { nested, rest } = Enum.split_with t, fn(x) ->
      Enum.take(x, length) == flag
    end

    module = get_docs_from_module(flag, h, nested)
    get_docs_from_modules(scope, rest, [module|acc])
  end

  defp get_docs_from_modules(_, [], acc) do
    List.reverse(acc)
  end

  defp get_docs_from_module(scope, segments, nested) do
    module   = :"__MAIN__.#{Enum.join segments, "."}"
    relative = Enum.join Enum.drop(segments, length(scope) - length(segments)), "."

    if match?({ :error,_ }, Code.ensure_loaded(module)), do:
      raise "module #{inspect module} is not defined"

    moduledoc = module.__info__(:moduledoc)

    unless moduledoc, do:
      raise "Module #{inspect module} was not compiled with flag --docs"

    docs = Enum.filter module.__info__(:docs), has_doc?(&1)

    ExDoc.Node.new(
      module: module,
      moduledoc: moduledoc,
      docs: docs,
      relative: relative,
      source: source_path(module),
      children: get_docs_from_modules(scope, nested, [])
    )
  end

  defp has_doc?({_, _, _, false}) do
    false
  end

  defp has_doc?({_, _, _, _doc}) do
    true
  end

  defp get_module_from_file(name, parts) do
    name = File.split :filename.rootname(name, ".beam")
    name -- parts
  end

  # TODO: This function needs to receive the project root level
  # as argument. Relying on this logic will break on next Erlang
  # release.
  defp source_path(module) do
    compile_info = module.__info__(:compile)
    compile_options = Keyword.get(compile_info, :options)
    compile_source  = Keyword.get(compile_info, :source)

    list_to_binary :lists.nthtail(length(compile_source) + 1,
                                  Keyword.get(compile_options, :source))
  end
end
