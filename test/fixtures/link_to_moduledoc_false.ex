defmodule LinkToModuledocFalse do
  @moduledoc """
  Links to the following modules/functions:

    - `ModuledocFalse`
    - `ModuledocFalse.foo/0`
    - `t:ModuledocFalse.t/0`
    - `Kernel.ParallelCompiler.require/2`
    - `Kernel.Doesnotexist`
    - `String`
    - `String.foo/0`
    - `t:String.t/0`
    - `String.Foo`
    - `Makeup`
    - `Doc.False.hello/0`
    - `Doc.Exist.hello/0`

  """

  @doc """
  Hello world.

  ## Examples

      iex> LinkToModuledocFalse.hello()
      :world

  """
  def hello do
    :world
  end
end
