defmodule CustomBehaviour do
  use Behaviour

  @doc """
  I should be first.
  """
  defcallback first(integer) :: integer

  @doc """
  This is a sample callback.
  """
  defcallback hello(integer) :: integer

  @doc """
  I should be last.
  """
  defcallback last(integer) :: integer
end
