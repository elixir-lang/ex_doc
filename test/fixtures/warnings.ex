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
