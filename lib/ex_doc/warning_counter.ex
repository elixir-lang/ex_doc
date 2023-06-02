defmodule ExDoc.WarningCounter do
  @moduledoc false

  use GenServer

  @type count :: non_neg_integer()
  @type counters_ref :: :counters.counters_ref()
  @typep caller :: pid() | :default
  @typep state :: %{:default => counters_ref(), caller() => counters_ref()}

  defguardp is_caller(term) when is_pid(term) or term == :default

  ###########################
  # Callback implementations

  @spec start_link(any()) :: GenServer.on_start()
  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, name: __MODULE__)
  end

  @impl GenServer
  @spec init(any()) :: {:ok, state}
  def init(_args) do
    counter_ref = new_counter()

    state = %{
      self() => counter_ref,
      default: counter_ref
    }

    {:ok, state}
  end

  @impl GenServer
  def handle_call({:count, caller}, _from, state) when is_caller(caller) do
    counter_ref = get_counter_ref(state, caller)
    count = :counters.get(counter_ref, 1)

    {:reply, count, state}
  end

  @impl GenServer
  def handle_info({:increment, caller}, state) when is_caller(caller) do
    counter_ref = get_counter_ref(state, caller)
    :counters.add(counter_ref, 1, 1)

    {:noreply, state}
  end

  def handle_info({:register_caller, caller}, state) when is_caller(caller) do
    counter_ref = new_counter()
    state = Map.put(state, caller, counter_ref)

    {:noreply, state}
  end

  def handle_info({:unregister_caller, caller}, state) when is_caller(caller) do
    state = Map.delete(state, caller)

    {:noreply, state}
  end

  #############
  # Public API

  @spec count(caller()) :: count()
  def count(caller \\ self()) do
    GenServer.call(__MODULE__, {:count, caller})
  end

  @spec increment() :: :ok
  def increment() do
    caller = self()
    send(__MODULE__, {:increment, caller})

    :ok
  end

  @spec register_caller(caller()) :: :ok
  def register_caller(caller) when is_caller(caller) do
    send(__MODULE__, {:register_caller, caller})
  end

  @spec unregister_caller(caller()) :: :ok
  def unregister_caller(caller) when is_caller(caller) do
    send(__MODULE__, {:unregister_caller, caller})
  end

  ##################
  # Private helpers

  defp new_counter() do
    :counters.new(1, [:atomics])
  end

  defp get_counter_ref(state, caller)
       when is_map(state) and is_caller(caller) and is_map_key(state, caller) do
    Map.fetch!(state, caller)
  end

  defp get_counter_ref(%{default: default_counter_ref} = state, caller)
       when is_map(state) and is_caller(caller) do
    callers = callers(caller)

    Enum.find_value(callers, default_counter_ref, fn the_caller ->
      Map.get(state, the_caller)
    end)
  end

  defp callers(pid) when is_pid(pid) do
    case Process.info(pid, :dictionary) do
      {:dictionary, dictionary} ->
        Keyword.get(dictionary, :"$callers", [])

      nil ->
        []
    end
  end
end
