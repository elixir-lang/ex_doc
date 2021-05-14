defmodule ExDoc.Language do
  @moduledoc false

  @callback module_data(module()) :: map()

  @callback function_data(entry :: tuple(), module_data :: map()) :: map()

  @callback callback_data(entry :: tuple(), module_data :: map()) :: map()

  @callback type_data(entry :: tuple(), spec :: term(), module_data :: map()) :: map()

  @callback typespec(spec :: term(), opts :: keyword()) :: term()

  def get(:elixir), do: ExDoc.Language.Elixir
  def get(:erlang), do: ExDoc.Language.Erlang
end
