defmodule ExDoc.Refs do
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
  #     visibility() :: :hidden | ::public | :undefined
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
  @typep visibility() :: :hidden | :public | :undefined

  @name __MODULE__

  use GenServer

  @spec start_link(any()) :: GenServer.on_start()
  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, name: @name)
  end

  @spec init(any()) :: {:ok, nil}
  def init(_) do
    :ets.new(@name, [:named_table, :public, :set])
    {:ok, nil}
  end

  @spec clear() :: :ok
  def clear() do
    :ets.delete_all_objects(@name)
    :ok
  end

  @spec get_visibility(ref()) :: visibility()
  def get_visibility(ref) do
    case lookup(ref) do
      [{^ref, visibility}] ->
        visibility

      [] ->
        case load(ref) do
          # when we only have exports, consider all types and callbacks refs as matching
          {:exports, entries} when elem(ref, 0) in [:type, :callback] ->
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
  def insert(entries) do
    true = :ets.insert(@name, entries)
    :ok
  end

  # Returns refs for `module` from the result of calling `Code.fetch_docs/1`.
  @doc false
  @spec from_chunk(module, tuple()) :: {:chunk | :exports | :none, [entry()]}
  def from_chunk(module, result) do
    case result do
      {:docs_v1, _, _, _, module_visibility, _, docs} ->
        module_visibility =
          if module_visibility == :hidden do
            :hidden
          else
            :public
          end

        entries =
          for {{kind, name, arity}, _, _, doc, metadata} <- docs do
            ref_visibility =
              if doc == :hidden or module_visibility == :hidden do
                :hidden
              else
                :public
              end

            for arity <- (arity - (metadata[:defaults] || 0))..arity do
              {{kind(kind), module, name, arity}, ref_visibility}
            end
          end

        entries = [{{:module, module}, module_visibility} | List.flatten(entries)]
        {:chunk, entries}

      {:error, :chunk_not_found} ->
        if Code.ensure_loaded?(module) do
          entries =
            for {name, arity} <- exports(module) do
              {{:function, module, name, arity}, :public}
            end

          entries = [{{:module, module}, :public} | entries]
          {:exports, entries}
        else
          entries = [{{:module, module}, :undefined}]
          {:none, entries}
        end

      _ ->
        entries = [{{:module, module}, :undefined}]
        {:none, entries}
    end
  end

  defp exports(module) do
    if function_exported?(module, :__info__, 1) do
      module.__info__(:functions) ++ module.__info__(:macros)
    else
      module.module_info(:exports)
    end
  end

  defp load({:module, module}) do
    from_chunk(module, ExDoc.Utils.Code.fetch_docs(module))
  end

  defp load({_kind, module, _name, _arity}) do
    load({:module, module})
  end

  defp kind(:macro), do: :function
  defp kind(:macrocallback), do: :callback
  defp kind(other), do: other
end
