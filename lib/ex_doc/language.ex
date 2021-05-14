defmodule ExDoc.Language do
  @moduledoc false

  @doc """
  Returns a map with module information.

  The map has the following keys:

    * `:id`

    * `:title`

    * `:type`

    * `:skip` - whether module should be skipped from generating the docs

    * `:extra_callback_types` - a list of types that are considered callbacks

  """
  @callback module_data(module()) :: map()

  @doc """
  Returns a map with function information.

  The map has the following keys:

    * `:doc_fallback` - if set, a 0-arity function that returns DocAST which
       will be used as fallback to empty docs on the function node

    * `:extra_annotations`

    * `:line_override` - if set, overrides the line where the code is located

    * `:specs` - a list of specs that will be later formatted by `c:typespec/2`

  """
  @callback function_data(entry :: tuple(), module_data :: map()) :: map()

  @doc """
  Returns a map with callback information.

  The map has the following keys:

    * `:actual_def` - `{name, arity}` of how the callback is actually represented
      in abstract code

    * `:line` - if set, returns the line where the code is located

    * `:signature_fallback` - if set, a 0-arity function that returns signature which
      which will be used as a fallback to empty signature on the callback node

    * `:specs` - a list of specs that will be later formatted by `c:typespec/2`

  """
  @callback callback_data(entry :: tuple(), module_data :: map()) :: map()

  @doc """
  Returns a map with type information.

  The map has the following keys:

    * `:spec` - a spec that will be later formatted by `c:typespec/2`

    * `:signature_fallback` - if set, a 0-arity function that returns signature which
      which will be used as a fallback to empty signature on the callback node

  """
  @callback type_data(entry :: tuple(), spec :: term(), module_data :: map()) :: map()

  @doc """
  Formats typespecs.
  """
  @callback typespec(spec :: term(), opts :: keyword()) :: iodata()

  def get(:elixir), do: ExDoc.Language.Elixir
  def get(:erlang), do: ExDoc.Language.Erlang
end
