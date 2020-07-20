defmodule OverlappingDefaults do
  @moduledoc """
  Overlapping default functions
  """

  @doc "Basic example"
  def overlapping_defaults(one, two) when is_list(two),
    do: {one, two}

  @doc "Third default arg overrides previous def clause"
  def overlapping_defaults(one, two, three \\ []),
    do: {one, two, three}

  def two_defaults(one, two) when is_atom(one) and is_atom(two),
    do: {one, two}

  @doc "Two default args"
  def two_defaults(one, two, three \\ [], four \\ [])
      when is_list(one) and is_list(two) and is_list(three) and is_list(four),
      do: {one, two, three, four}

  def special_case(one, two) when is_atom(one) and is_atom(two),
    do: {one, two}

  @doc "This function defines an arity that is less than the one in the previous clause"
  def special_case(one, two \\ [], three \\ [], four \\ [])
      when is_list(one) and is_list(two) and is_list(three) and is_list(four),
      do: {one, two, three, four}

  defmacro in_the_middle(foo, bar) when is_list(foo) and is_list(bar),
    do: quote(do: {unquote(foo), unquote(bar)})

  @doc "default arg is in the middle"
  defmacro in_the_middle(foo, bar \\ Baz, baz),
    do: quote(do: {unquote(foo), unquote(bar), unquote(baz)})
end
