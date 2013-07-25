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
  def get_docs(files, config) when is_list(files) do
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
      modules:   nest_modules([], Enum.sort(remaining), [], config),
      records:   nest_modules([], Enum.sort(records), [], config),
      protocols: nest_modules([], Enum.sort(protocols), [], config)
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
  defp nest_modules(scope, [h|t], acc, config) do
    flag   = scope ++ elem(h, 0)
    length = length(flag)

    { nested, rest } = Enum.split_while t, fn({ x, _, _ }) ->
      Enum.take(x, length) == flag
    end

    module = get_module(flag, h, nested, config)
    nest_modules(scope, rest, [module|acc], config)
  end

  defp nest_modules(_, [], acc, _) do
    Enum.reverse(acc)
  end

  # Get all the information from the module and compile
  # it, also looping through its children. If there is
  # an error while retrieving the information (like
  # the module is not available or it was not compiled
  # with --docs flag), we raise an exception.
  defp get_module(scope, { segments, module, type }, children, config) do
    relative = Enum.join Enum.drop(segments, length(scope) - length(segments)), "."

    case module.__info__(:moduledoc) do
      { line, moduledoc } -> nil
      _ -> raise "Module #{inspect module} was not compiled with flag --docs"
    end

    source_url  = config.source_url_pattern
    source_path = source_path(module, config)

    docs = Enum.filter_map module.__info__(:docs), has_doc?(&1, type),
      get_function(&1, source_path, source_url)

    if type == :behaviour do
      callbacks = Kernel.Typespec.beam_callbacks(module)

      docs = Enum.map(module.__behaviour__(:docs),
        get_callback(&1, source_path, source_url, callbacks)) ++ docs
    end

    ExDoc.ModuleNode[
      id: inspect(module),
      line: line,
      module: module,
      type: type,
      moduledoc: moduledoc,
      docs: docs,
      relative: relative,
      source: source_link(source_path, source_url, line),
      children: nest_modules(scope, children, [], config)
    ]
  end

  # Skip docs explicitly marked as false
  defp has_doc?({_, _, _, _, false}, _) do
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

  defp get_function(function, source_path, source_url) do
    { { name, arity }, line, type, signature, doc } = function

    ExDoc.FunctionNode[
      name: name,
      arity: arity,
      id: "#{name}/#{arity}",
      doc: doc,
      signature: signature,
      source: source_link(source_path, source_url, line),
      type: type,
      line: line
    ]
  end

  defp get_callback(callback, source_path, source_url, callbacks) do
    { { name, arity } = tuple, line, kind, doc } = callback
    { _, signatures } = List.keyfind(callbacks, tuple, 0, { nil, [] })

    if signature = Enum.first(signatures) do
      { :::, _, [{ ^name, _, signature }, _] } = Kernel.Typespec.spec_to_ast(name, signature)
    end

    ExDoc.FunctionNode[
      name: name,
      arity: arity,
      id: "#{name}/#{arity}",
      doc: doc || nil,
      signature: signature,
      source: source_link(source_path, source_url, line),
      type: :defcallback,
      line: line
    ]
  end

  defp get_module_from_file(name) do
    name   = Path.basename name, ".beam"
    module = binary_to_atom name

    unless Code.ensure_loaded?(module), do:
      raise(Error, message: "module #{inspect module} is not defined/available")

    case module.__info__(:moduledoc) do
      { _, false } ->
        nil
      { _, _moduledoc } ->
        { Module.split(name), module, detect_type(module) }
      _ ->
        raise(Error, message: "module #{inspect module} was not compiled with flag --docs")
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
      function_exported?(module, :__impl__, 1) -> :impl
      defines_behaviour?(module) -> :behaviour
      true -> nil
    end
  end

  defp defines_behaviour?(module) do
    if function_exported?(module, :__behaviour__, 1) do
      try do
        module.__behaviour__(:callbacks)
        true
      rescue
        _ -> false
      end
    else
      false
    end
  end

  defp source_link(_source_path, nil, _line), do: nil

  defp source_link(source_path, source_url, line) do
    source_url = Regex.replace(%r/%{path}/, source_url, source_path)
    Regex.replace(%r/%{line}/, source_url, to_binary(line))
  end

  defp source_path(module, config) do
    source = module.__info__(:compile)[:source]
    Path.relative_to source, config.source_root
  end
end
