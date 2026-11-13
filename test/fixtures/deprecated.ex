defmodule Deprecated do
  @moduledoc """
  A module for testing deprecations.
  """
  @moduledoc deprecated: "Use example_1/0 or String.to_atom/1 instead"

  defmacro example_1, do: 1

  @doc "A function with links in deprecation"
  @deprecated "Use example_1/0 or String.to_atom/1 instead"
  def example_with_links, do: :ok

  @doc "A function without links in deprecation"
  @deprecated "Use something else instead"
  def example_without_links, do: :ok

  @deprecated "Use example_1/0 or String.to_atom/1 instead"
  def example_without_docs, do: nil
end
