defmodule Warnings do
  @moduledoc """
  `Warnings.bar/0`
  """

  @moduledoc deprecated: "Use something without warnings"

  @typedoc """
  `Warnings.bar/0`
  """
  @type t() :: :ok

  @doc """
  `Warnings.bar/0`
  """
  @callback handle_foo() :: :ok

  @doc """
  `Warnings.bar/0`
  """
  def foo(), do: :ok
end

defmodule Warnings.Submodule do
  @moduledoc """
  Refer to:
  - [this file](unknown.md)
  - [this module](`UknownModule`)
  - [this function](`CompiledWithDocs.function/0`)
  - [this callback](`c:CompiledWithDocs.callback/1`)
  - [this type](`t:CompiledWithDocs.type/2`)

  Refer to with Elixir namespace:
  - [this module](`Elixir.UknownModule`)
  - [this function](`Elixir.CompiledWithDocs.function/0`)
  - [this callback](`c:Elixir.CompiledWithDocs.callback/1`)
  - [this type](`t:Elixir.CompiledWithDocs.type/2`)

  """
end
