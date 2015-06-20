defmodule CustomBehaviourOne do
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
  defmacrocallback bye(integer) :: integer
end

defmodule CustomBehaviourImpl do
  @behaviour CustomBehaviourOne
  @behaviour CustomBehaviourTwo

  def hello(i), do: i

  @doc "A doc for this so it doesn't use 'Callback implementation for'"
  defmacro bye(i), do: i
end
