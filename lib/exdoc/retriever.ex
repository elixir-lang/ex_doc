defrecord ExDoc.ModuleNode, module: nil, relative: nil, moduledoc: nil,
  docs: [], source: nil, children: [], type: nil, id: nil, line: 0

defrecord ExDoc.FunctionNode, name: nil, arity: 0, id: nil,
  doc: [], source: nil, type: nil, line: 0

defmodule ExDoc.Retriever do
  import :erlang, only: [function_exported: 3]

  defexception Error, message: nil

  @doc """
  This function receives a bunch of .beam file paths and
  the directory they are relative to and return a list of
  `ExDoc.ModuleNode`. Those nodes are nested (child modules
  can be found under the function children of each node) and
  contain all the required information already processed.
  """
  def get_docs(files, relative_to) when is_list(files) and is_binary(relative_to) do
    # Split the relative directory into parts
    parts = File.split "#{relative_to}/__MAIN__"

    # Then we get all the module names as a list of binaries.
    # For example, the module Foo.Bar.Baz will be represented
    # as ["Foo", "Bar", "Baz"]
    modules = Enum.map files, get_module_from_file(&1, parts)

    # Split each type
    protocols = Enum.filter modules, match?({ _, _, x } when x in [:protocol, :impl], &1)
    records   = Enum.filter modules, match?({ _, _, x } when x in [:record, :exception], &1)
    remaining = Enum.filter modules, match?({ _, _, nil }, &1)

    # Sort the modules and return the list of nodes
    [
      modules:   nest_modules([], Enum.qsort(remaining), []),
      records:   nest_modules([], Enum.qsort(records), []),
      protocols: nest_modules([], Enum.qsort(protocols), [])
    ]
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
  defp nest_modules(scope, [h|t], acc) do
    flag   = scope ++ elem(h, 1)
    length = length(flag)

    { nested, rest } = Enum.split_with t, fn({ x, _, _ }) ->
      Enum.take(x, length) == flag
    end

    module = get_module(flag, h, nested)
    nest_modules(scope, rest, [module|acc])
  end

  defp nest_modules(_, [], acc) do
    List.reverse(acc)
  end

  # Get all the information from the module and compile
  # it, also looping through its children. If there is
  # an error while retrieving the information (like
  # the module is not available or it was not compiled
  # with --docs flag), we raise an exception.
  defp get_module(scope, { segments, module, type }, children) do
    relative = Enum.join Enum.drop(segments, length(scope) - length(segments)), "."

    case module.__info__(:moduledoc) do
    match: { line, moduledoc }
      nil
    else:
      raise "Module #{inspect module} was not compiled with flag --docs"
    end

    source_path = source_path(module)
    docs = Enum.filter_map module.__info__(:docs), has_doc?(&1, type), get_function(&1, source_path)

    ExDoc.ModuleNode[
      id: inspect(module),
      line: line,
      module: module,
      type: type,
      moduledoc: moduledoc,
      docs: docs,
      relative: relative,
      source: source_link(source_path, line),
      children: nest_modules(scope, children, [])
    ]
  end

  # Skip docs explicitly marked as false
  defp has_doc?({_, _, _, false}, _) do
    false
  end

  # Skip exception specific functions
  defp has_doc?({{:update___exception__, _}, _, _, _}, :exception) do
    false
  end

  # Skip everything starting with __ if it does not have explicit docs
  defp has_doc?({{name, _}, _, _, nil}, _) do
    hd(atom_to_list(name)) != ?_
  end

  # Everything else is ok
  defp has_doc?(_, _) do
    true
  end

  defp get_function({ { name, arity }, line, type, doc }, source_path) do
    ExDoc.FunctionNode[
      name: name,
      arity: arity,
      id: "#{name}/#{arity}",
      doc: doc,
      source: source_link(source_path, line),
      type: type,
      line: line
    ]
  end

  defp get_module_from_file(name, parts) do
    name = File.split :filename.rootname(name, ".beam")

    segments = name -- parts
    module   = :"__MAIN__.#{Enum.join segments, "."}"

    if match?({ :error,_ }, Code.ensure_loaded(module)), do:
      raise Error, message: "module #{inspect module} is not defined/available"

    case module.__info__(:moduledoc) do
    match: { _, false }
      nil
    match: { _, _moduledoc }
      { segments, module, detect_type(module) }
    else:
      raise Error, message: "module #{inspect module} was not compiled with flag --docs"
    end
  end

  # Detect if a module is an exception, record,
  # protocol, implementation or simply a module
  defp detect_type(module) do
    if function_exported(module, :__record__, 1) do
      if function_exported(module, :__exception__, 1),
        do: :exception, else: :record
    elsif: function_exported(module, :__protocol__, 1)
      :protocol
    elsif: function_exported(module, :__impl__, 0)
      :impl
    else:
      nil
    end
  end

  # TODO The project URL needs to be configurable
  defp source_link(source_path, line) do
    "https://github.com/elixir-lang/elixir/blob/master/#{source_path}#L#{line}"
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
