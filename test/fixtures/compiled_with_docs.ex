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

  @doc "Another example with &mdash; & &ndash; (— & –)"
  @doc since: "1.3.0"
  defmacro example_1, do: 1

  @doc "A simple guard"
  defguard is_zero(number) when number == 0

  @doc """
  Does example action.

  ### Examples
  """
  @doc purpose: :example
  def example_with_h3, do: 1

  @deprecated "Use something else instead"
  def example_without_docs, do: nil

  # Check that delegate autogenerate docs
  defdelegate flatten(hello), to: List

  defmodule Nested do
  end
end
