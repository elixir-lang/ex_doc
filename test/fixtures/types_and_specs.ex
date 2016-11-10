defmodule TypesAndSpecs do
  defmodule Sub do
    @type t :: any
  end

  @moduledoc """
  Types and tests fixture.
  """

  @typedoc "A public type"
  @type public(t) :: {t, String.t, Sub.t, opaque, :ok | :error}
  @typep private :: any
  @opaque opaque :: {Dict.t}
  @type ref :: {:binary.part, public(any)}

  @spec add(integer, opaque) :: integer
  def add(x, _), do: x + x

  @spec minus(integer, integer) :: integer
  defp minus(x, y), do: x - y

  @spec macro_spec(any) :: {:ok, any}
  defmacro macro_spec(v), do: {:ok, v}

  @spec priv_macro_spec(any) :: {:no, any}
  defmacrop priv_macro_spec(v), do: {:no, v}

  # This is just to ignore warnings about unused private types/functions.
  @spec ignore(private) :: integer
  def ignore(_), do: priv_macro_spec(minus(0, 0))
end
