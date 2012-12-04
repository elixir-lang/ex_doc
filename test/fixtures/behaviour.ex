defmodule CustomBehaviour do
  use Behaviour

  @doc """
  This is a sample callback.
  """
  defcallback hello(integer) :: integer
end