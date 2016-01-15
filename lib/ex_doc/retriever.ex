defmodule ExDoc.ModuleNode do
  @moduledoc """
  Structure that represents a *module*
  """

  defstruct id: nil, module: nil, moduledoc: nil,
    docs: [], typespecs: [], source: nil, type: nil
end

defmodule ExDoc.FunctionNode do
  @moduledoc """
  Structure that holds all the elements of an individual *function*
  """

  defstruct id: nil, name: nil, arity: 0, doc: [],
    source: nil, type: nil, signature: nil, specs: []
end

defmodule ExDoc.TypeNode do
  @moduledoc """
  Structure that holds all the elements of an individual *type*
  """

  defstruct id: nil, name: nil, arity: 0, type: nil,
    spec: nil, doc: nil, signature: nil
end

defmodule ExDoc.Retriever.Error do
  @moduledoc """
  Structure that hold the message of a given exception
  """
  defexception [:message]
end

defmodule ExDoc.Retriever do
  @moduledoc """
  Functions to extract documentation information from modules.
  """

  alias ExDoc.Retriever.Error
  alias Kernel.Typespec

  @doc """
  Extract documentation from all modules in the specified directory
  """
  @spec docs_from_dir(Path.t, %ExDoc.Config{}) :: [%ExDoc.ModuleNode{}]
  def docs_from_dir(dir, config) when is_binary(dir) do
    files = Path.wildcard Path.expand("Elixir.*.beam", dir)
    docs_from_files(files, config)
  end

  @doc """
  Extract documentation from all modules in the specified list of files
  """
  @spec docs_from_files([Path.t], %ExDoc.Config{}) :: [%ExDoc.ModuleNode{}]
  def docs_from_files(files, config) when is_list(files) do
    files
    |> Enum.map(&filename_to_module(&1))
    |> docs_from_modules(config)
  end

  @doc """
  Extract documentation from all modules in the list `modules`
  """
  @spec docs_from_modules([atom], %ExDoc.Config{}) :: [%ExDoc.ModuleNode{}]
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
    if function_exported?(module, :__info__, 1) do
      case Code.get_docs(module, :moduledoc) do
        {_line, false} ->
          nil
        {_, _} ->
          module
        nil ->
          raise("module #{inspect module} was not compiled with flag --docs")
      end
    else
      IO.puts(:stderr, "module #{inspect module} does not export __info__/1")
      nil
    end
  end

  defp generate_node(nil, _, _), do: nil

  defp generate_node(module, type, config) do
    source_url  = config.source_url_pattern
    source_path = source_path(module, config)

    specs = get_specs(module)
    impls = get_impls(module)
    abst_code = get_abstract_code(module)

    moduledoc = get_moduledoc(module)
    line = find_actual_line(abst_code, module, :module)

    docs = get_docs(type, module, source_path, source_url, specs, impls, abst_code) ++
           get_callbacks(type, module, source_path, source_url, abst_code)

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

  defp get_moduledoc(module) do
    {_, moduledoc} = Code.get_docs(module, :moduledoc)
    moduledoc
  end

  defp get_abstract_code(module) do
    {^module, binary, _file} = :code.get_object_code(module)
    case :beam_lib.chunks(binary, [:abstract_code]) do
      {:ok, {_, [{:abstract_code, {_vsn, abstract_code}}]}} ->
        abstract_code
      _otherwise -> []
    end
  end

  defp get_docs(type, module, source_path, source_url, specs, impls, abst_code) do
    docs = Enum.sort_by Code.get_docs(module, :docs), &elem(&1, 0)
    for doc <- docs, doc?(doc, type) do
      get_function(doc, source_path, source_url, specs, impls, abst_code)
    end
  end

  # Skip impl_for and impl_for! for protocols
  defp doc?({{name, _}, _, _, _, nil}, :protocol) when name in [:impl_for, :impl_for!] do
    false
  end

  # Skip docs explicitly marked as false
  defp doc?({_, _, _, _, false}, _) do
    false
  end

  # Skip default docs if starting with _
  defp doc?({{name, _}, _, _, _, nil}, _type) do
    hd(Atom.to_char_list(name)) != ?_
  end

  # Everything else is ok
  defp doc?(_, _) do
    true
  end

  defp get_function(function, source_path, source_url, all_specs, cb_impls, abst_code) do
    {{name, arity}, doc_line, type, signature, doc} = function
    function = actual_def(name, arity, type)
    line = find_actual_line(abst_code, function, :function) || doc_line

    behaviour = Map.get(cb_impls, {name, arity})

    if is_nil(doc) && behaviour do
      doc = "Callback implementation for `c:#{inspect behaviour}.#{name}/#{arity}`."
    end

    specs = all_specs
            |> Map.get(function, [])
            |> Enum.map(&Typespec.spec_to_ast(name, &1))

    %ExDoc.FunctionNode{
      id: "#{name}/#{arity}",
      name: name,
      arity: arity,
      doc: doc,
      signature: get_call_signature(name, signature),
      specs: specs,
      source: source_link(source_path, source_url, line),
      type: type
    }
  end

  defp get_callbacks(:behaviour, module, source_path, source_url, abst_code) do
    callbacks = Enum.into(Typespec.beam_callbacks(module) || [], %{})

    docs =
      if function_exported?(module, :__behaviour__, 1) do
        module.__behaviour__(:docs)
      else
        Code.get_docs(module, :all)[:callback_docs]
      end

    docs = Enum.sort_by docs || [], &elem(&1, 0)
    Enum.map(docs, &get_callback(&1, source_path, source_url, callbacks, abst_code))
  end

  defp get_callbacks(_, _, _, _, _), do: []

  defp get_callback(callback, source_path, source_url, callbacks, abst_code) do
    {{name, arity}, _, kind, doc} = callback
    function = actual_def(name, arity, kind)
    line = find_actual_line(abst_code, function, :callback)

    # TODO: Remove defcallback and defmacrocallback
    # once we no longer supported __behaviour__
    kind =
      case kind do
        :def -> :callback
        :defmacro -> :macrocallback
        other -> other
      end

    specs =
      callbacks
      |> Map.get(function, [])
      |> Enum.map(&Typespec.spec_to_ast(name, &1))

    %ExDoc.FunctionNode{
      id: "#{name}/#{arity}",
      name: name,
      arity: arity,
      doc: doc || nil,
      signature: get_typespec_signature(hd(specs), arity),
      specs: specs,
      source: source_link(source_path, source_url, line),
      type: kind
    }
  end

  defp get_typespec_signature({:when, _, [{:::, _, [{name, meta, args}, _]}, _]}, arity) do
    Macro.to_string {name, meta, strip_types(args, arity)}
  end

  defp get_typespec_signature({:::, _, [{name, meta, args}, _]}, arity) do
    Macro.to_string {name, meta, strip_types(args, arity)}
  end

  defp get_typespec_signature({name, meta, args}, arity) do
    Macro.to_string {name, meta, strip_types(args, arity)}
  end

  defp strip_types(args, arity) do
    args
    |> Enum.take(-arity)
    |> Enum.with_index()
    |> Enum.map(fn
      {{:::, _, [left, _]}, i} -> to_var(left, i)
      {{:|, _, _}, i}          -> to_var({}, i)
      {left, i}                -> to_var(left, i)
    end)
  end

  defp to_var({name, meta, _}, _) when is_atom(name),
    do: {name, meta, nil}
  defp to_var({:<<>>, _, _}, _),
    do: {:binary, [], nil}
  defp to_var({:%{}, _, _}, _),
    do: {:map, [], nil}
  defp to_var({:{}, _, _}, _),
    do: {:tuple, [], nil}
  defp to_var({_, _}, _),
    do: {:tuple, [], nil}
  defp to_var(integer, _) when is_integer(integer),
    do: {:integer, [], nil}
  defp to_var(float, _) when is_integer(float),
    do: {:float, [], nil}
  defp to_var(list, _) when is_list(list),
    do: {:list, [], nil}
  defp to_var(atom, _) when is_atom(atom),
    do: {:atom, [], nil}
  defp to_var(_, i),
    do: {:"arg#{i}", [], nil}

  defp get_call_signature(name, args) do
    cond do
      name in [:__aliases__, :__block__] ->
        "#{name}(args)"
      name in [:__ENV__, :__MODULE__, :__DIR__, :__CALLER__, :"%", :"%{}"] ->
        "#{name}"
      true ->
        Macro.to_string {name, [], args}
    end
  end

  defp actual_def(name, arity, :macrocallback) do
    actual_def(name, arity, :defmacro)
  end
  defp actual_def(name, arity, :defmacro) do
    {String.to_atom("MACRO-" <> to_string(name)), arity + 1}
  end
  defp actual_def(name, arity, _), do: {name, arity}

  defp find_actual_line(abst_code, function, :callback) do
    abst_code
    |> Enum.find(&match?({:attribute, _, :callback, {^function, _}}, &1))
    |> elem(1)
  end

  defp find_actual_line(abst_code, name, :module) do
    abst_code
    |> Enum.find(&match?({:attribute, _, :module, ^name}, &1))
    |> elem(1)
  end

  defp find_actual_line(abst_code, {name, arity}, :function) do
    case Enum.find(abst_code, &match?({:function, _, ^name, ^arity, _}, &1)) do
      nil -> nil
      tuple -> elem(tuple, 1)
    end
  end

  # Detect if a module is an exception, struct,
  # protocol, implementation or simply a module
  defp detect_type(module) do
    cond do
      function_exported?(module, :__struct__, 0) and
        match?(%{__exception__: true}, module.__struct__) -> :exception
      function_exported?(module, :__protocol__, 1) -> :protocol
      function_exported?(module, :__impl__, 1) -> :impl
      function_exported?(module, :behaviour_info, 1) -> :behaviour
      true -> :module
    end
  end

  # Returns a dict of {name, arity} -> spec.
  defp get_specs(module) do
    Enum.into(Typespec.beam_specs(module) || [], %{})
  end

  # Returns a dict of {name, arity} -> behaviour.
  defp get_impls(module) do
    for behaviour <- behaviours_implemented_by(module),
        callback <- callbacks_defined_by(behaviour),
        do: {callback, behaviour},
        into: %{}
  end

  defp callbacks_defined_by(module) do
    :attributes
    |> module.module_info
    |> Enum.filter_map(&match?({:callback, _}, &1), fn {_, [{t,_}|_]} -> t end)
  end

  defp behaviours_implemented_by(module) do
    :attributes
    |> module.module_info
    |> Stream.filter(&match?({:behaviour, _}, &1))
    |> Stream.map(fn {_, l} -> l end)
    |> Enum.concat()
  end

  defp get_types(module) do
    all  = Typespec.beam_types(module) || []
    docs = try do
      Enum.into(Code.get_docs(module, :type_docs) || [], %{},
                fn {typedoc, _, _, doc} -> {typedoc, doc} end)
    rescue
      _ -> Enum.into(Typespec.beam_typedocs(module) || [], %{})
    end

    types =
      for {type, {name, _, args} = tuple} <- all, type != :typep do
        spec  = process_type_ast(Typespec.type_to_ast(tuple), type)
        arity = length(args)
        doc   = docs[{name, arity}]
        %ExDoc.TypeNode{
          id: "#{name}/#{arity}",
          name: name,
          arity: arity,
          type: type,
          spec: spec,
          doc: doc,
          signature: get_typespec_signature(spec, arity)
        }
      end

    Enum.sort_by types, &{&1.name, &1.arity}
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
