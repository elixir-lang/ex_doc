defmodule ExDoc.WarningCounter do
  @moduledoc false

  use Agent

  def start_link(_opts) do
    Agent.start_link(fn -> 0 end, name: __MODULE__)
  end

  def count do
    Agent.get(__MODULE__, & &1)
  end

  def increment do
    Agent.update(__MODULE__, &(&1 + 1))
  end

  def reset do
    Agent.update(__MODULE__, fn _ -> 0 end)
  end
end
