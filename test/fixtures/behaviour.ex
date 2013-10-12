defmodule CustomBehaviour do
  use Behaviour

  @doc """
  This is a sample callback.
  """
  defcallback hello(integer) :: integer
end

defmodule CustomBehaviourTwo do
  use Behaviour

  @doc """
  This is a different sample callback.
  """
  defcallback bye(integer) :: integer
end

defmodule CustomBehaviourImpl do
  @behaviour CustomBehaviour
  @behaviour CustomBehaviourTwo

  def hello(i), do: i

  @doc "A doc for this so it doesn't use 'Callback implementation of'"
  def bye(i), do: i
end
