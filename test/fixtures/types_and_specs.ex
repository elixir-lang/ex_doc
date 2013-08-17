defmodule TypesAndSpecs do
  @moduledoc """
  Types and tests fixture.
  """

  @type public(t) :: { t, String.t, :ok | :error }
  @typep private :: any
  @opaque opaque :: Dict.t

  @spec add(integer, integer) :: integer
  def add(x, y), do: x + y

  @spec minus(integer, integer) :: integer
  defp minus(x, y), do: x - y
end
