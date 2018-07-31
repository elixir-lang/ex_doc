defmodule CompiledWithDocs do
  @moduledoc """
  moduledoc

  ## Example ☃ Unicode > escaping
      CompiledWithDocs.example

  ### Example H3 heading

  example
  """

  @doc "Some struct"
  defstruct [:field]

  @doc "Some example"
  @doc purpose: :example
  @deprecated "Use something else instead"
  def example(foo, bar \\ Baz), do: bar.baz(foo)

  @doc "Another example"
  @doc since: "1.3.0"
  defmacro example_1, do: 1

  @doc "A simple guard"
  # TODO: remove explicit guard: true when ~> 1.8
  @doc guard: true
  defguard is_zero(number) when number == 0

  @doc """
  Does example action.

  ### Examples
  """
  @doc purpose: :example
  def example_with_h3, do: 1

  @doc purpose: :example
  def example_without_docs, do: nil

  defmodule Nested do
  end
end
