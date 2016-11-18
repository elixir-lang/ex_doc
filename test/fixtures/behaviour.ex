defmodule CustomBehaviourOne do
  # Defining a struct should not affect the behaviour
  defstruct [:a, :b]

  @doc """
  This is a sample callback.
  """
  @callback hello(integer) :: integer
  @callback greet(integer | String.t) :: integer
end

defmodule CustomBehaviourTwo do
  @doc """
  This is a different sample callback.
  """
  @macrocallback bye(integer) :: integer
end

defmodule CustomBehaviourImpl do
  @behaviour CustomBehaviourOne
  @behaviour CustomBehaviourTwo

  def hello(i), do: i
  def greet(i), do: i

  @doc "A doc for this so it doesn't use 'Callback implementation for'"
  defmacro bye(i), do: i
end
