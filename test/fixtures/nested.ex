defmodule UndefParent.Nested do
  @moduledoc """
  This is a nested module
  """

  @doc """
  This is a function with arity 2
  """
  def example(_, _) do
  end

  @doc false
  def example_without_docs do
  end
end

defmodule UndefParent.Undocumented do
  @moduledoc false
end
