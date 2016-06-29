defmodule CompiledWithDocs do
  @moduledoc """
  moduledoc

  ## Example ☃ Unicode
      CompiledWithDocs.example
  """

  @doc "Some example"
  def example(foo, bar \\ Baz), do: bar.baz(foo)

  @doc "Another example"
  defmacro example_1, do: 1

  def example_without_docs, do: nil

  defmodule Nested do
  end
end
