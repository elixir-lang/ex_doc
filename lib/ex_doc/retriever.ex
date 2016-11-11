defmodule ExDoc.ModuleNode do
  @moduledoc """
  Structure that represents a *module*
  """

  defstruct id: nil, module: nil, doc: nil, doc_line: nil,
    docs: [], typespecs: [], source_path: nil, source_url: nil, type: nil

  @type t :: %__MODULE__{
    id: nil | String.t,
    module: nil | String.t,
    docs: list(),
    doc: nil | String.t,
    doc_line: non_neg_integer(),
    typespecs: list(),
    source_path: nil | String.t,
    source_url: nil | String.t,
    type: nil | String.t
  }
end

defmodule ExDoc.FunctionNode do
  @moduledoc """
  Structure that holds all the elements of an individual *function*
  """

  defstruct id: nil, name: nil, arity: 0, defaults: [], doc: [],
    type: nil, signature: nil, specs: [], annotations: [],
    doc_line: nil, source_path: nil, source_url: nil

  @type t :: %__MODULE__{
    id: nil | String.t,
    name: nil | String.t,
    arity: non_neg_integer,
    defaults: non_neg_integer,
    doc: String.t(),
    doc_line: non_neg_integer,
    source_path: nil | String.t,
    source_url: nil | String.t,
    type: nil | String.t,
    signature: nil | String.t,
    specs: list(),
    annotations: list()
  }
end

defmodule ExDoc.TypeNode do
  @moduledoc """
  Structure that holds all the elements of an individual *type*
  """

  defstruct id: nil, name: nil, arity: 0, type: nil, doc_line: nil,
    source_path: nil, source_url: nil, spec: nil, doc: nil,
    signature: nil, annotations: []

  @type t :: %__MODULE__{
    id: nil | String.t,
    name: nil | String.t,
    arity: non_neg_integer,
    type: nil | String.t,
    spec: nil | String.t,
    doc: nil | String.t,
    doc_line: non_neg_integer,
    signature: nil | String.t,
    source_url: nil | String.t,
    source_path: nil | String.t,
    annotations: list()
  }
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
  Extract documentation from all modules in the specified directory or directories.
  """
  @spec docs_from_dir(Path.t | [Path.t], ExDoc.Config.t) :: [ExDoc.ModuleNode.t]
  def docs_from_dir(dir, config) when is_binary(dir) do
    pattern = if config.filter_prefix, do: "Elixir.#{config.filter_prefix}*.beam", else: "*.beam"
    files = Path.wildcard Path.expand(pattern, dir)
    docs_from_files(files, config)
  end
  def docs_from_dir(dirs, config) when is_list(dirs) do
    Enum.flat_map(dirs, &docs_from_dir(&1, config))
  end

  @doc """
  Extract documentation from all modules in the specified list of files
  """
  @spec docs_from_files([Path.t], ExDoc.Config.t) :: [ExDoc.ModuleNode.t]
  def docs_from_files(files, config) when is_list(files) do
    files
    |> Enum.map(&filename_to_module(&1))
    |> docs_from_modules(config)
  end

  @doc """
  Extract documentation from all modules in the list `modules`
  """
  @spec docs_from_modules([atom], ExDoc.Config.t) :: [ExDoc.ModuleNode.t]
  def docs_from_modules(modules, config) when is_list(modules) do
    modules
    |> Enum.map(&get_module(&1, config))
    |> Enum.filter(&(&1))
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

    if export_docs?(module) do
      generate_node(module, type, config)
    end
  end

  # Special case required for Elixir
  defp export_docs?(:elixir_bootstrap), do: false

  defp export_docs?(module) do
    if function_exported?(module, :__info__, 1) do
      case Code.get_docs(module, :moduledoc) do
        {_line, false} ->
          false
        {_, _} ->
          true
        nil ->
          raise("module #{inspect module} was not compiled with flag --docs")
      end
    else
      false
    end
  end

  defp generate_node(module, type, config) do
    source_url  = config.source_url_pattern
    source_path = source_path(module, config)

    specs = get_specs(module)
    impls = get_impls(module)
    abst_code = get_abstract_code(module)

    {doc_line, moduledoc} = Code.get_docs(module, :moduledoc)
    line = find_actual_line(abst_code, module, :module) || doc_line

    docs = get_docs(type, module, source_path, source_url, specs, impls, abst_code) ++
           get_callbacks(type, module, source_path, source_url, abst_code)

    id =
      case inspect(module) do
        ":" <> inspected -> inspected
        inspected -> inspected
      end

    %ExDoc.ModuleNode{
      id: id,
      module: module,
      type: type,
      docs: docs,
      doc: moduledoc,
      doc_line: doc_line,
      typespecs: get_types(module, source_path, source_url, abst_code),
      source_path: source_path,
      source_url: source_link(source_path, source_url, line)
    }
  end

  # Helpers

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

    doc =
      if is_nil(doc) && behaviour do
        "Callback implementation for `c:#{inspect behaviour}.#{name}/#{arity}`."
      else
        doc
      end

    specs = all_specs
            |> Map.get(function, [])
            |> Enum.map(&Typespec.spec_to_ast(name, &1))
            |> Enum.reverse()

    %ExDoc.FunctionNode{
      id: "#{name}/#{arity}",
      name: name,
      arity: arity,
      doc: doc,
      doc_line: doc_line,
      defaults: get_defaults(signature, name, arity),
      signature: get_call_signature(name, signature),
      specs: specs,
      source_path: source_path,
      source_url: source_link(source_path, source_url, line),
      type: type
    }
  end

  defp get_defaults(signature, name, arity) do
    case Enum.count(signature, &match?({:\\, _, [_, _]}, &1)) do
      0 -> []
      defaults -> for default <- (arity - defaults)..(arity - 1), do: "#{name}/#{default}"
    end
  end

  defp get_callbacks(:behaviour, module, source_path, source_url, abst_code) do
    optional_callbacks = get_optional_callbacks(abst_code)
    (Code.get_docs(module, :callback_docs) || [])
    |> Enum.sort_by(&elem(&1, 0))
    |> Enum.map(&get_callback(&1, source_path, source_url, optional_callbacks, abst_code))
  end

  defp get_callbacks(_, _, _, _, _), do: []

  defp get_optional_callbacks(abst_code) do
    for {:attribute, _, :optional_callbacks, callbacks} <- abst_code,
        callback <- callbacks,
        do: callback
  end

  defp get_callback(callback, source_path, source_url, optional_callbacks, abst_code) do
    {{name, arity}, doc_line, kind, doc} = callback
    function = actual_def(name, arity, kind)

    {:attribute, anno, :callback, {^function, specs}} =
      Enum.find(abst_code, &match?({:attribute, _, :callback, {^function, _}}, &1))
    line  = anno_line(anno) || doc_line
    specs = Enum.map(specs, &Typespec.spec_to_ast(name, &1))

    name_and_arity =
      case kind do
        :callback -> {name, arity}
        :macrocallback -> {:"MACRO-#{name}", arity + 1}
      end

    annotations =
      if name_and_arity in optional_callbacks do
        ["optional"]
      else
        []
      end

    %ExDoc.FunctionNode{
      id: "#{name}/#{arity}",
      name: name,
      arity: arity,
      doc: doc || nil,
      doc_line: doc_line,
      signature: get_typespec_signature(hd(specs), arity),
      specs: specs,
      source_path: source_path,
      source_url: source_link(source_path, source_url, line),
      type: kind,
      annotations: annotations,
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

  defp find_actual_line(abst_code, name, :module) do
    abst_code
    |> Enum.find(&match?({:attribute, _, :module, ^name}, &1))
    |> elem(1)
    |> anno_line()
  end

  defp find_actual_line(abst_code, {name, arity}, :function) do
    case Enum.find(abst_code, &match?({:function, _, ^name, ^arity, _}, &1)) do
      nil -> nil
      tuple -> tuple |> elem(1) |> anno_line()
    end
  end

  defp anno_line(line) when is_integer(line), do: line
  defp anno_line(anno), do: :erl_anno.line(anno)

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
    module
    |> Kernel.Typespec.beam_callbacks()
    |> Kernel.||([]) # In case the module source is not available
    |> Keyword.keys
  end

  defp behaviours_implemented_by(module) do
    for {:behaviour, list} <- module.module_info(:attributes),
        behaviour <- list,
        do: behaviour
  end

  defp get_types(module, source_path, source_url, abst_code) do
    exported = for {:attribute, _, :export_type, types} <- abst_code, do: types
    exported = :lists.flatten(exported)

    (Code.get_docs(module, :type_docs) || [])
    |> Enum.filter(&elem(&1, 0) in exported)
    |> Enum.sort_by(&elem(&1, 0))
    |> Enum.map(&get_type(&1, source_path, source_url, abst_code))
  end

  defp get_type(type, source_path, source_url, abst_code) do
    {{name, arity}, doc_line, _, doc} = type

    {:attribute, anno, type, spec} =
      Enum.find(abst_code, &match?({:attribute, _, type, {^name, _, args}}
                                     when type in [:opaque, :type] and length(args) == arity, &1))

    spec = process_type_ast(Typespec.type_to_ast(spec), type)
    line = anno_line(anno) || doc_line

    %ExDoc.TypeNode{
      id: "#{name}/#{arity}",
      name: name,
      arity: arity,
      type: type,
      spec: spec,
      doc: doc || nil,
      doc_line: doc_line,
      signature: get_typespec_signature(spec, arity),
      source_path: source_path,
      source_url: source_link(source_path, source_url, line)
    }
  end

  defp source_link(_source_path, nil, _line), do: nil

  defp source_link(source_path, source_url, line) do
    source_url = Regex.replace(~r/%{path}/, source_url, source_path)
    Regex.replace(~r/%{line}/, source_url, to_string(line))
  end

  defp source_path(module, config) do
    source = module.__info__(:compile)[:source] |> String.Chars.to_string

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
