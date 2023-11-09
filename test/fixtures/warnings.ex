defmodule Warnings do
  @moduledoc """
  moduledoc `Warnings.bar/0`
  """

  @moduledoc deprecated: "Use something without warnings"

  @typedoc """
  typedoc `Warnings.bar/0`
  """
  @type t() :: :ok

  @doc """
  doc callback `Warnings.bar/0`
  """
  @callback handle_foo() :: :ok

  @doc """
  doc `Warnings.bar/0`
  """
  def foo(), do: :ok
end
