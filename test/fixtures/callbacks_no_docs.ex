defmodule CallbacksNoDocs do

  @callback connect(params :: map, String.t) :: {:ok, String.t} | :error
  @callback id(String.t) :: String.t | nil

end
