defrecord ExDoc.Node, module: nil, relative: nil, moduledoc: nil,
  docs: [], source: nil, children: []

defmodule ExDoc.Retriever do
  @doc """
  This function receives a bunch of .beam file paths and
  the directory they are relative to and return a list of
  `ExDoc.Node`. Those nodes are nested (child modules can
  be found under the function children of each node) and
  contain all the information requires by the modules.
  """
  def get_docs(files, relative_to) when is_list(files) and is_binary(relative_to) do
    # Split the relative directory into parts
    parts = File.split "#{relative_to}/__MAIN__"

    # Then we get all the module names as a list of binaries.
    # For example, the module Foo.Bar.Baz will be represented
    # as ["Foo", "Bar", "Baz"]
    modules = Enum.map files, get_module_from_file(&1, parts)

    # Sort the modules and return the list of nodes
    get_docs_from_modules([], Enum.qsort(modules), [])
  end

  # Helpers

  # Goes through each module in the list. Since the list
  # is ordered, nested modules can be found by looking
  # ahead the list under the given scope. For example,
  # imagine the list is made by the modules:
  #
  #     [
  #       ["Foo"],
  #       ["Foo", "Bar"],
  #       ["Last"]
  #     ]
  #
  # On the first interaction, it will see ["Foo"]
  # and look ahead the list noticing that the next
  # module starts with "Foo" and consequently is a
  # child.
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

  # Get all the information from the module and compile
  # it, also looping through its children. If there is
  # an error while retrieving the information (like
  # the module is not available or it was not compiled
  # with --docs flag), we raise an exception.
  defp get_docs_from_module(scope, segments, children) do
    module   = :"__MAIN__.#{Enum.join segments, "."}"
    relative = Enum.join Enum.drop(segments, length(scope) - length(segments)), "."

    if match?({ :error,_ }, Code.ensure_loaded(module)), do:
      raise "module #{inspect module} is not defined/available"

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
      children: get_docs_from_modules(scope, children, [])
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

  # Get the source of the compiled module. Due to a bug in Erlang
  # R15 and before, we need to look for the source first in the
  # options and then into the real source.
  #
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
