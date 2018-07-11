defmodule CallbacksNoDocs do
  @callback connect(params :: map, String.t) :: {:ok, String.t} | :error

  @doc deprecated: "Use another id", since: "1.3.0"
  @callback id(String.t) :: String.t | nil

  @optional_callbacks id: 1
end
