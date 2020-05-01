defmodule ExDoc.Refs do
  @moduledoc false

  # A read-through cache of documentation references.
  #
  # The cache consists of entries:
  #
  #     entry() :: {ref(), public? :: boolean()}
  #
  #     ref() ::
  #       {:module, module()}
  #       | {kind(), module(), name :: atom(), arity()}
  #
  #     kind() :: :function | :callback | :type
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

  def public?(ref) do
    case lookup(ref) do
      [{^ref, true}] ->
        true

      [{^ref, false}] ->
        false

      [] ->
        case load(ref) do
          # when we only have exports, consider all types and callbacks refs as matching
          {:exports, entries} when elem(ref, 0) in [:type, :callback] ->
            :ok = insert([{ref, true} | entries])
            true

          {_, entries} ->
            :ok = insert(entries)
            {ref, true} in entries
        end
    end
  end

  defp lookup(ref) do
    :ets.lookup(@name, ref)
  rescue
    _ ->
      [{ref, false}]
  end

  def insert(entries) do
    true = :ets.insert(@name, entries)
    :ok
  end

  # Returns refs for `module` from the result of calling `Code.fetch_docs/1`.
  @doc false
  def from_chunk(module, result) do
    case result do
      {:docs_v1, _, _, _, :hidden, _, _} ->
        {:chunk, [{{:module, module}, false}]}

      {:docs_v1, _, _, _, _, _, docs} ->
        entries =
          for {{kind, name, arity}, _, _, doc, metadata} <- docs do
            tag = doc != :hidden

            for arity <- (arity - (metadata[:defaults] || 0))..arity do
              {{kind(kind), module, name, arity}, tag}
            end
          end

        entries = [{{:module, module}, true} | List.flatten(entries)]
        {:chunk, entries}

      {:error, :chunk_not_found} ->
        if Code.ensure_loaded?(module) do
          entries =
            for {name, arity} <- exports(module) do
              {{:function, module, name, arity}, true}
            end

          entries = [{{:module, module}, true} | entries]
          {:exports, entries}
        else
          entries = [{{:module, module}, false}]
          {:none, entries}
        end

      _ ->
        entries = [{{:module, module}, false}]
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
