defrecord ExDoc.Node, name: nil, moduledoc: nil, docs: [],
  source: nil, children: []

defmodule ExDoc.Retriever do
  def get_docs(files, relative_to) do
    modules = Enum.map files, get_module_from_file(&1, "#{relative_to}/__MAIN__")
    get_docs_from_modules(:lists.sort(modules))
  end

  # Helpers

  defp get_docs_from_modules([h|t]) do
    [get_docs_from_module(h)|get_docs_from_modules(t)]
  end

  defp get_docs_from_modules([]) do
    []
  end

  defp get_docs_from_module({ _, module }) do
    moduledoc = module.__info__(:moduledoc)
    docs      = Enum.filter module.__info__(:docs), has_doc?(&1)

    ExDoc.Node.new(
      name: inspect(module),
      source: source_path(module),
      moduledoc: moduledoc,
      docs: docs
    )
  end

  defp has_doc?({_, _, _, false}) do
    false
  end

  defp has_doc?({_, _, _, _doc}) do
    true
  end

  defp get_module_from_file(name, relative_to) do
    name = File.split :filename.rootname(name, '.beam')
    relative = File.split relative_to
    hierarchy = :lists.subtract name, relative
    segments = Enum.join hierarchy, "."
    { length(hierarchy), :"__MAIN__.#{segments}" }
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
