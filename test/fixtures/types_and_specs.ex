defmodule TypesAndSpecs do
  defmodule Sub do
    @type t :: any
  end
  
  @moduledoc """
  Types and tests fixture.
  """

  @type public(t) :: { t, String.t, Sub.t, opaque, :ok | :error }
  @typep private :: any
  @opaque opaque :: {Dict.t}
  @type ref :: { :binary.part, public(any) }

  @spec add(integer, opaque) :: integer
  def add(x, _), do: x + x 

  @spec minus(integer, integer) :: integer
  defp minus(x, y), do: x - y
end
