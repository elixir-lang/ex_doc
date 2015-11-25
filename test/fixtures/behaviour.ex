defmodule CustomBehaviourOne do
  use Behaviour

  # Defining a struct should not affect the behaviour
  defstruct [:a, :b]

  @doc """
  This is a sample callback.
  """
  defcallback hello(integer) :: integer
  defcallback greet(integer | String.t) :: integer
end

defmodule CustomBehaviourTwo do
  use Behaviour

  @doc """
  This is a different sample callback.
  """
  defmacrocallback bye(integer) :: integer
end

defmodule CustomBehaviourImpl do
  @behaviour CustomBehaviourOne
  @behaviour CustomBehaviourTwo

  def hello(i), do: i
  def greet(i), do: i

  @doc "A doc for this so it doesn't use 'Callback implementation for'"
  defmacro bye(i), do: i
end
