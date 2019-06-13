defmodule ModuledocFalse do
  @moduledoc false

  @type t :: term

  @doc """
  Foo is the new bar
  """
  def foo do
    :foo
  end
end
