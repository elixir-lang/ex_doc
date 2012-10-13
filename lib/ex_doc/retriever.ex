defrecord ExDoc.ModuleNode, module: nil, relative: nil, moduledoc: nil,
  docs: [], source: nil, children: [], type: nil, id: nil, line: 0

defrecord ExDoc.FunctionNode, name: nil, arity: 0, id: nil,
  doc: [], source: nil, type: nil, line: 0, signature: nil

defmodule ExDoc.Retriever do
  defexception Error, message: nil

  @doc """
  This function receives a bunch of .beam file paths and
  the directory they are relative to and return a list of
  `ExDoc.ModuleNode`. Those nodes are nested (child modules
  can be found under the function children of each node) and
  contain all the required information already processed.
  """
  def get_docs(files, project_url) when is_list(files) do
    # Then we get all the module names as a list of binaries.
    # For example, the module Foo.Bar.Baz will be represented
    # as ["Foo", "Bar", "Baz"]
    modules = Enum.map files, get_module_from_file(&1)

    # Split each type
    protocols = Enum.filter modules, match?({ _, _, x } when x in [:protocol, :impl], &1)
    records   = Enum.filter modules, match?({ _, _, x } when x in [:record, :exception], &1)
    remaining = Enum.filter modules, match?({ _, _, x } when x in [nil, :behaviour], &1)

    # Sort the modules and return the list of nodes
    [
      modules:   nest_modules([], Enum.qsort(remaining), [], project_url),
      records:   nest_modules([], Enum.qsort(records), [], project_url),
      protocols: nest_modules([], Enum.qsort(protocols), [], project_url)
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
  defp nest_modules(scope, [h|t], acc, project_url) do
    flag   = scope ++ elem(h, 0)
    length = length(flag)

    { nested, rest } = Enum.split_while t, fn({ x, _, _ }) ->
      Enum.take(x, length) == flag
    end

    module = get_module(flag, h, nested, project_url)
    nest_modules(scope, rest, [module|acc], project_url)
  end

  defp nest_modules(_, [], acc, _) do
    Enum.reverse(acc)
  end

  # Get all the information from the module and compile
  # it, also looping through its children. If there is
  # an error while retrieving the information (like
  # the module is not available or it was not compiled
  # with --docs flag), we raise an exception.
  defp get_module(scope, { segments, module, type }, children, project_url) do
    relative = Enum.join Enum.drop(segments, length(scope) - length(segments)), "."

    case module.__info__(:moduledoc) do
      { line, moduledoc } -> nil
      _ -> raise "Module #{inspect module} was not compiled with flag --docs"
    end

    source_path = source_path(module)
    docs = Enum.filter_map module.__info__(:docs), has_doc?(&1, type),
      get_function(&1, source_path, project_url)

    ExDoc.ModuleNode[
      id: inspect(module),
      line: line,
      module: module,
      type: type,
      moduledoc: moduledoc,
      docs: docs,
      relative: relative,
      source: source_link(project_url, source_path, line),
      children: nest_modules(scope, children, [], project_url)
    ]
  end

  # Skip docs explicitly marked as false
  defp has_doc?({_, _, _, _, false}, _) do
    false
  end

  # Skip docs by default for implementations
  defp has_doc?({_, _, _, _, nil}, :impl) do
    false
  end

  # Skip default docs if starting with _
  defp has_doc?({{name, _}, _, _, _, nil}, _type) do
    hd(atom_to_list(name)) != ?_
  end

  # Everything else is ok
  defp has_doc?(_, _) do
    true
  end

  defp get_function(function, source_path, project_url) do
    { { name, arity }, line, type, signature, doc } = function

    ExDoc.FunctionNode[
      name: name,
      arity: arity,
      id: "#{name}/#{arity}",
      doc: doc,
      signature: signature,
      source: source_link(project_url, source_path, line),
      type: type,
      line: line
    ]
  end

  defp get_module_from_file(name) do
    name   = File.basename name, ".beam"
    module = binary_to_atom name

    unless Code.ensure_loaded?(module), do:
      raise Error, message: "module #{inspect module} is not defined/available"

    case module.__info__(:moduledoc) do
      { _, false } ->
        nil
      { _, _moduledoc } ->
        { Module.split(name), module, detect_type(module) }
      _ ->
        raise Error, message: "module #{inspect module} was not compiled with flag --docs"
    end
  end

  # Detect if a module is an exception, record,
  # protocol, implementation or simply a module
  defp detect_type(module) do
    cond do
      function_exported?(module, :__record__, 1) ->
        case module.__record__(:fields) do
          [{ :__exception__, :__exception__}|_] -> :exception
          _ -> :record
        end
      function_exported?(module, :__protocol__, 1) -> :protocol
      function_exported?(module, :__impl__, 0) -> :impl
      module_callbacks(module) != [] -> :behaviour
      true -> nil
    end
  end

  defp module_callbacks(module) do
    if function_exported?(module, :behaviour_info, 1) do
      try do
        module.behaviour_info(:callbacks)
      rescue
        _ -> []
      end
    else
      []
    end
  end

  defp source_link(project_url, source_path, line) do
    project_url = Regex.replace(%r/%{path}/, project_url, source_path)
    Regex.replace(%r/%{line}/, project_url, to_binary(line))
  end

  # Get the source of the compiled module. Due to a bug in Erlang
  # R15 and before, we need to look for the source first in the
  # options and then into the real source.
  defp source_path(module) do
    compile_info = module.__info__(:compile)
    compile_source  = Keyword.get(compile_info, :source)

    list_to_binary :lists.nthtail(String.length(File.cwd!) + 1,
                                  compile_source)
  end
end
