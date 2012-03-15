defmodule CompiledWithDocs do
  @moduledoc """
  moduledoc

  ## Example
      CompiledWithDocs.example
  """

  @doc "Some example"
  def example, do: 1

  @doc "Another example"
  defmacro example_1, do: 1
end
