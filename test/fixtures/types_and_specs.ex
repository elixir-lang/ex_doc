defmodule TypesAndSpecs do
  defmodule Sub do
    @type t :: any
  end

  @type t :: %__MODULE__{
          a01: String.t(),
          a02: String.t(),
          a03: String.t(),
          a04: String.t(),
          a05: String.t(),
          a06: String.t(),
          a07: String.t(),
          a08: String.t(),
          a09: String.t(),
          a10: String.t(),
          a11: String.t(),
          a12: String.t(),
          a13: String.t(),
          a14: String.t(),
          a15: String.t(),
          a16: String.t(),
          a17: String.t(),
          a18: String.t(),
          a19: String.t(),
          a20: String.t(),
          a21: String.t(),
          a22: String.t(),
          a23: String.t(),
          a24: String.t(),
          a25: String.t(),
          a26: String.t(),
          a27: String.t(),
          a28: String.t(),
          a29: String.t(),
          a30: String.t(),
          a31: String.t(),
          a32: String.t(),
          a33: String.t()
        }

  defstruct [
    :a01,
    :a02,
    :a03,
    :a04,
    :a05,
    :a06,
    :a07,
    :a08,
    :a09,
    :a10,
    :a11,
    :a12,
    :a13,
    :a14,
    :a15,
    :a16,
    :a17,
    :a18,
    :a19,
    :a20,
    :a21,
    :a22,
    :a23,
    :a24,
    :a25,
    :a26,
    :a27,
    :a28,
    :a29,
    :a30,
    :a31,
    :a32,
    :a33
  ]

  @moduledoc """
  Types and tests fixture.

  Basic type: `t:atom/0`.
  Special form: `import/2`.
  """

  @typedoc "A public type"
  @type public(t) :: {t, String.t(), Sub.t(), opaque, :ok | :error}
  @typep private :: any
  @opaque opaque :: {Dict.t()}
  @type ref :: {:binary.part(), public(any)}
  @typedoc false
  @type internal :: any

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
