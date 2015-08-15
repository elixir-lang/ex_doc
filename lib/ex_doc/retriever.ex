defmodule ExDoc.ModuleNode do
  defstruct id: nil, module: nil, moduledoc: nil,
    docs: [], typespecs: [], source: nil, type: nil
end

defmodule ExDoc.FunctionNode do
  defstruct id: nil, name: nil, arity: 0, doc: [],
    source: nil, type: nil, signature: nil, specs: []
end

defmodule ExDoc.TypeNode do
  defstruct id: nil, name: nil, arity: 0, type: nil,
    spec: nil, doc: nil
end

defmodule ExDoc.Retriever.Error do
  defexception [:message]
end

defmodule ExDoc.Retriever do
  @moduledoc """
  Functions to extract documentation information from modules.
  """

  alias ExDoc.Retriever.Error

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
    |> Enum.map(&filename_to_module(&1))
    |> docs_from_modules(config)
  end

  @doc """
  Extract documentation from all modules in the list `modules`
  """
  def docs_from_modules(modules, config) when is_list(modules) do
    modules
    |> Enum.map(&get_module(&1, config))
    |> Enum.filter(fn(x) -> x end)
    |> Enum.sort(&(&1.id <= &2.id))
  end

  defp filename_to_module(name) do
    name = Path.basename name, ".beam"
    String.to_atom name
  end

  # Get all the information from the module and compile
  # it. If there is an error while retrieving the information (like
  # the module is not available or it was not compiled
  # with --docs flag), we raise an exception.
  defp get_module(module, config) do
    unless Code.ensure_loaded?(module), do:
      raise(Error, message: "module #{inspect module} is not defined/available")

    type = detect_type(module)

    module
    |> verify_module()
    |> generate_node(type, config)
  end

  defp verify_module(module) do
    case Code.get_docs(module, :moduledoc) do
      {_line, false} ->
        nil
      {_, _} ->
        module
      nil ->
        raise(Error, message: "module #{inspect module} was not compiled with flag --docs")
    end
  end

  defp generate_node(nil, _, _), do: nil

  defp generate_node(module, type, config) do
    source_url  = config.source_url_pattern
    source_path = source_path(module, config)

    specs = Enum.into(Kernel.Typespec.beam_specs(module) || [], %{})
    impls = callbacks_implemented_by(module)

    docs = Enum.filter_map Code.get_docs(module, :docs), &has_doc?(&1, type),
                           &get_function(&1, source_path, source_url, specs, impls)

    if type == :behaviour do
      callbacks = Enum.into(Kernel.Typespec.beam_callbacks(module) || [], %{})

      inner =
        if function_exported?(module, :__behaviour__, 1) do
          module.__behaviour__(:docs)
        else
          Code.get_docs(module, :all)[:callback_docs]
        end

      docs = docs ++ Enum.map(inner || [], &get_callback(&1, source_path, source_url, callbacks))
    end

    {line, moduledoc} = Code.get_docs(module, :moduledoc)

    %ExDoc.ModuleNode{
      id: inspect(module),
      module: module,
      type: type,
      moduledoc: moduledoc,
      docs: docs,
      typespecs: get_types(module),
      source: source_link(source_path, source_url, line)
    }
  end

  # Helpers

  # Skip impl_for and impl_for! for protocols
  defp has_doc?({{name, _}, _, _, _, nil}, :protocol) when name in [:impl_for, :impl_for!] do
    false
  end

  # Skip docs explicitly marked as false
  defp has_doc?({_, _, _, _, false}, _) do
    false
  end

  # Skip default docs if starting with _
  defp has_doc?({{name, _}, _, _, _, nil}, _type) do
    hd(Atom.to_char_list(name)) != ?_
  end

  # Everything else is ok
  defp has_doc?(_, _) do
    true
  end

  defp spec_name(name, arity, :defmacro), do: {String.to_atom("MACRO-" <> to_string(name)), arity + 1}
  defp spec_name(name, arity, _), do: {name, arity}

  defp get_function(function, source_path, source_url, all_specs, cb_impls) do
    {{name, arity}, line, type, signature, doc} = function
    behaviour = Dict.get(cb_impls, {name, arity})

    doc =
      if is_nil(doc) && behaviour do
        "Callback implementation for `c:#{inspect behaviour}.#{name}/#{arity}`."
      else
        doc
      end

    specs = all_specs
            |> Dict.get(spec_name(name, arity, type), [])
            |> Enum.map(&Kernel.Typespec.spec_to_ast(name, &1))

    %ExDoc.FunctionNode{
      id: "#{name}/#{arity}",
      name: name,
      arity: arity,
      doc: doc,
      signature: get_signature(name, signature),
      specs: specs,
      source: source_link(source_path, source_url, line),
      type: type
    }
  end

  defp get_callback(callback, source_path, source_url, callbacks) do
    {{name, arity}, line, kind, doc} = callback

    specs = Dict.get(callbacks, {name, arity}, [])
            |> Enum.map(&Kernel.Typespec.spec_to_ast(name, &1))

    %ExDoc.FunctionNode{
      id: "#{name}/#{arity}",
      name: name,
      arity: arity,
      doc: doc || nil,
      signature: "#{name}/#{arity}",
      specs: specs,
      source: source_link(source_path, source_url, line),
      type: :"#{kind}callback"
    }
  end

  defp get_signature(name, args) do
    cond do
      name in [:__aliases__, :__block__] ->
        "#{name}(args)"
      name in [:__ENV__, :__MODULE__, :__DIR__, :__CALLER__, :"%", :"%{}"] ->
        "#{name}"
      true ->
        Macro.to_string { name, 0, args }
    end
  end

  # Detect if a module is an exception, struct,
  # protocol, implementation or simply a module
  defp detect_type(module) do
    cond do
      function_exported?(module, :__struct__, 0) ->
        case module.__struct__ do
          %{__exception__: true} -> :exception
          _ -> nil
        end
      function_exported?(module, :__protocol__, 1) -> :protocol
      function_exported?(module, :__impl__, 1) -> :impl
      function_exported?(module, :behaviour_info, 1) -> :behaviour
      true -> nil
    end
  end

  # Returns a dict of { name, arity } -> [ behaviour_module ].
  defp callbacks_implemented_by(module) do
    behaviours_implemented_by(module)
    |> Enum.map(fn behaviour -> Enum.map(callbacks_of(behaviour), &{ &1, behaviour }) end)
    |> Enum.reduce(%{}, &Enum.into/2)
  end

  defp callbacks_of(module) do
    module.module_info(:attributes)
    |> Enum.filter_map(&match?({ :callback, _ }, &1), fn {_, [{t,_}|_]} -> t end)
  end

  defp behaviours_implemented_by(module) do
    module.module_info(:attributes)
    |> Stream.filter(&match?({ :behaviour, _ }, &1))
    |> Stream.map(fn {_, l} -> l end)
    |> Enum.concat()
  end

  defp get_types(module) do
    all  = Kernel.Typespec.beam_types(module) || []
    docs = Enum.into(Kernel.Typespec.beam_typedocs(module) || [], %{})

    for { type, { name, _, args } = tuple } <- all, type != :typep do
      spec  = process_type_ast(Kernel.Typespec.type_to_ast(tuple), type)
      arity = length(args)
      doc   = docs[{ name, arity }]
      %ExDoc.TypeNode{
        id: "#{name}/#{arity}",
        name: name,
        arity: arity,
        type: type,
        spec: spec,
        doc: doc
      }
    end
  end

  defp source_link(_source_path, nil, _line), do: nil

  defp source_link(source_path, source_url, line) do
    source_url = Regex.replace(~r/%{path}/, source_url, source_path)
    Regex.replace(~r/%{line}/, source_url, to_string(line))
  end

  defp source_path(module, config) do
    source = module.__info__(:compile)[:source]

    if root = config.source_root do
      Path.relative_to(source, root)
    else
      source
    end
  end

  # Cut off the body of an opaque type while leaving it on a normal type.
  defp process_type_ast({:::, _, [d|_]}, :opaque), do: d
  defp process_type_ast(ast, _), do: ast
end
