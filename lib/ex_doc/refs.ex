defmodule ExDoc.Refs do
  alias ExDoc.Utils
  import Utils.Code, only: [ref_visibility: 1, ref_visibility: 2]

  @moduledoc false

  # A read-through cache of documentation references.
  #
  # The cache consists of entries:
  #
  #     entry() :: {ref(), visibility()}
  #
  #     ref() ::
  #       {:module, module()}
  #       | {kind(), module(), name :: atom(), arity()}
  #
  #     kind() :: :function | :callback | :type
  #
  #     visibility() :: :hidden | :private | :public | :undefined
  #
  # A given ref is always associated with a module. If we don't have a ref in the cache we fetch
  # the module's chunk and fill in the cache. This means that if we keep asking for references
  # that don't exist (e.g.: `Foo.bar/9`, `Foo.bar/10`, etc) we will keep fetching the chunk;
  # this could be optimized in the future however users will get a warning and hopefully fix
  # their broken refs.
  #
  # If the module does not have a chunk, we fill in the cache with it's exports. Anytime we ask
  # such module whether it has given types or callbacks we need to say "yes" (and cache that)
  # as we can't know.

  @typep entry() :: {ref(), visibility()}
  @typep ref() ::
           {:module, module()}
           | {kind(), module(), name :: atom(), arity()}
  @typep kind() :: :function | :callback | :type
  @typep visibility() :: :hidden | :private | :public | :undefined

  @name __MODULE__

  use GenServer

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, name: @name)
  end

  def init(_) do
    :ets.new(@name, [:named_table, :public, :set])
    {:ok, nil}
  end

  def clear() do
    :ets.delete_all_objects(@name)
    :ok
  end

  @spec get_visibility(ref()) :: visibility()
  def get_visibility(ref) when is_tuple(ref) do
    kind = elem(ref, 0)

    case lookup(ref) do
      [{^ref, visibility}] ->
        visibility

      [] ->
        case load(ref) do
          # when module has been compiled with no docs, consider all types and callbacks refs as matching
          {:definitions, entries} when kind in [:type, :callback] ->
            :ok = insert([{ref, :public} | entries])
            :public

          {_, entries} ->
            :ok = insert(entries)

            Enum.find_value(entries, :undefined, fn
              {^ref, visibility} ->
                visibility

              _ ->
                false
            end)
        end
    end
  end

  @spec public?(ref()) :: boolean
  def public?(ref) do
    get_visibility(ref) == :public
  end

  @spec lookup(ref()) :: [entry()]
  def lookup(ref) do
    :ets.lookup(@name, ref)
  rescue
    _ ->
      [{ref, :undefined}]
  end

  @spec insert([entry()]) :: :ok
  def insert([] = _entries),
    do: :ok

  def insert(entries) do
    true = :ets.insert(@name, entries)
    :ok
  end

  # Returns refs for `module` from the result of calling `Code.fetch_docs/1`.
  @doc false
  @spec from_chunk(module, tuple()) :: {:chunk | :definitions | :none, [entry()]}
  def from_chunk(module, fetch_docs_result) do
    case fetch_docs_result do
      {:docs_v1, _, _, _, module_doc, _, docs} ->
        module_visibility = ref_visibility(module_doc)

        entries =
          for {{kind, name, arity}, _, _, doc, metadata} <- docs do
            ref_visibility = ref_visibility(doc, module_visibility)

            for arity <- (arity - (metadata[:defaults] || 0))..arity do
              {{kind(kind), module, name, arity}, ref_visibility}
            end
          end

        entries = [
          {{:module, module}, module_visibility}
          | List.flatten([
              entries,
              private_types(module, module_visibility),
              callbacks(module, module_visibility),
              private_functions_macros(module, module_visibility)
            ])
        ]

        {:chunk, entries}

      {:error, :chunk_not_found} ->
        if Code.ensure_loaded?(module) do
          module_visibility = :public

          entries =
            Enum.concat([
              [{{:module, module}, module_visibility}],
              exports(module, module_visibility),
              private_types(module, module_visibility),
              callbacks(module, module_visibility),
              private_functions_macros(module, module_visibility)
            ])

          {:definitions, entries}
        else
          entries = [{{:module, module}, :undefined}]
          {:none, entries}
        end

      _ ->
        entries = [{{:module, module}, :undefined}]
        {:none, entries}
    end
  end

  defp exports(module, module_visibility) when is_atom(module) and is_atom(module_visibility) do
    exports =
      if function_exported?(module, :__info__, 1) do
        module.__info__(:functions) ++ module.__info__(:macros)
      else
        module.module_info(:exports)
      end

    ref_visibility = ref_visibility(:public, module_visibility)

    for {name, arity} <- exports do
      {{:function, module, name, arity}, ref_visibility}
    end
  end

  defp private_types(module, module_visibility)
       when is_atom(module) and is_atom(module_visibility) do
    refs =
      case Code.Typespec.fetch_types(module) do
        {:ok, types} ->
          Enum.reduce(types, [], fn
            {:typep, {type, _, args}}, acc ->
              [{type, length(args)} | acc]

            _, acc ->
              acc
          end)

        :error ->
          []
      end

    ref_visibility = ref_visibility(:private, module_visibility)

    for {name, arity} <- refs do
      {{:type, module, name, arity}, ref_visibility}
    end
  end

  defp callbacks(module, module_visibility) when is_atom(module) and is_atom(module_visibility) do
    case Code.Typespec.fetch_callbacks(module) do
      {:ok, callbacks} ->
        ref_visibility = ref_visibility(:public, module_visibility)

        for {function, arity} <- callbacks do
          {{:callback, module, function, arity}, ref_visibility}
        end

      :error ->
        []
    end
  end

  def private_functions_macros(module, module_visibility)
      when is_atom(module) and is_atom(module_visibility) do
    case Utils.Code.fetch_definitions(module, module_visibility) do
      {:ok, definitions} ->
        for {{function, arity}, ref_visibility} <- definitions do
          {{:function, module, function, arity}, ref_visibility}
        end

      :error ->
        []
    end
  end

  defp load({:module, module}) do
    from_chunk(module, Utils.Code.fetch_docs(module))
  end

  defp load({_kind, module, _name, _arity}) do
    load({:module, module})
  end

  defp kind(:macro), do: :function
  defp kind(:macrocallback), do: :callback
  defp kind(other), do: other
end
