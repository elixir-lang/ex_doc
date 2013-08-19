defrecord ExDoc.ModuleNode, id: nil, module: nil, moduledoc: nil,
  docs: [], typespecs: [], source: nil, type: nil

defrecord ExDoc.FunctionNode, id: nil, name: nil, arity: 0,
  doc: [], source: nil, type: nil, signature: nil, specs: nil

defrecord ExDoc.TypeNode, id: nil, name: nil, arity: 0, type: nil, spec: nil

defmodule ExDoc.Retriever do
  @moduledoc """
  Functions to extract documentation information from modules.
  """

  defexception Error, message: nil

  @doc """
  Extract documentation from all modules in the specified directory
  """
  def docs_from_dir(dir, config) do
    files = Path.wildcard Path.expand("Elixir.*.beam", dir)
    docs_from_files(files, config)
  end

  @doc """
  Extract documentation from all modules in the specified list of files
  """
  def docs_from_files(files, config) when is_list(files) do
    files
      |> Enum.map(&get_module_from_file(&1))
      |> Enum.filter(fn(x) -> x end)
      |> docs_from_modules(config)
  end

  @doc """
  Extract documentation from all modules in the list `modules`
  """
  def docs_from_modules(modules, config) when is_list(modules) do
    Enum.map(modules, &get_module(&1, config)) |> Enum.sort
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
        module
      _ ->
        raise(Error, message: "module #{inspect module} was not compiled with flag --docs")
    end
  end

  # Get all the information from the module and compile
  # it. If there is an error while retrieving the information (like
  # the module is not available or it was not compiled
  # with --docs flag), we raise an exception.
  defp get_module(module, config) do
    case module.__info__(:moduledoc) do
      { line, moduledoc } -> nil
      _ -> raise "Module #{inspect module} was not compiled with flag --docs"
    end

    source_url  = config.source_url_pattern
    source_path = source_path(module, config)

    specs = Kernel.Typespec.beam_specs(module)
    types = get_types(module)

    type = detect_type(module)
    docs = Enum.filter_map module.__info__(:docs), &has_doc?(&1, type),
                           &get_function(&1, source_path, source_url, specs)

    if type == :behaviour do
      callbacks = Kernel.Typespec.beam_callbacks(module)
      docs = Enum.map(module.__behaviour__(:docs),
        &get_callback(&1, source_path, source_url, callbacks)) ++ docs
    end

    ExDoc.ModuleNode[
      id: inspect(module),
      module: module,
      type: type,
      moduledoc: moduledoc,
      docs: docs,
      typespecs: types,
      source: source_link(source_path, source_url, line),
    ]
  end

  # Helpers

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

  defp get_function(function, source_path, source_url, all_specs) do
    { { name, arity }, line, type, signature, doc } = function

    specs = Dict.get(all_specs, { name, arity }, [])
            |> Enum.map(&Kernel.Typespec.spec_to_ast(name, &1))

    ExDoc.FunctionNode[
      id: "#{name}/#{arity}",
      name: name,
      arity: arity,
      doc: doc,
      signature: signature,
      specs: specs,
      source: source_link(source_path, source_url, line),
      type: type
    ]
  end

  defp get_callback(callback, source_path, source_url, callbacks) do
    { { name, arity } = tuple, line, _kind, doc } = callback
    { _, signatures } = List.keyfind(callbacks, tuple, 0, { nil, [] })

    if signature = Enum.first(signatures) do
      { :::, _, [{ ^name, _, signature }, _] } = Kernel.Typespec.spec_to_ast(name, signature)
    end

    ExDoc.FunctionNode[
      id: "#{name}/#{arity}",
      name: name,
      arity: arity,
      doc: doc || nil,
      signature: signature,
      source: source_link(source_path, source_url, line),
      type: :defcallback
    ]
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

  defp get_types(module) do
    all = Kernel.Typespec.beam_types(module)

    lc { type, { name, _, args } = tuple } inlist all, type != :typep do
      spec  = process_type_ast(Kernel.Typespec.type_to_ast(tuple), type)
      arity = length(args)
      ExDoc.TypeNode[id: "#{name}/#{arity}", name: name, arity: arity, type: type, spec: spec]
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

  # Cut off the body of an opaque type while leaving it on a normal type.
  defp process_type_ast({:::, _, [d|_]}, :opaque), do: d
  defp process_type_ast(ast, _), do: ast
end
