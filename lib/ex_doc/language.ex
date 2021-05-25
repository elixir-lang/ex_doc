defmodule ExDoc.Language do
  @moduledoc false

  @typep spec_ast() :: term()

  @doc """
  Returns a map with module information.

  The map has the following keys:

    * `:id` - module page name

    * `:title` - module display title

    * `:type` - module type

    * `:skip` - whether module should be skipped from generating the docs

    * `:extra_callback_types` - a list of types that are considered callbacks

  """
  @callback module_data(module()) :: data
            when data: %{
                   id: String.t(),
                   title: String.t(),
                   type: atom() | nil,
                   skip: boolean(),
                   extra_callback_types: [atom()]
                 }

  @doc """
  Returns a map with function information.

  The map has the following keys:

    * `:doc_fallback` - if set, a 0-arity function that returns DocAST which
       will be used as fallback to empty docs on the function node

    * `:extra_annotations`

    * `:line_override` - if set, overrides the line where the code is located

    * `:specs` - a list of specs that will be later formatted by `c:typespec/2`

  """
  @callback function_data(entry :: tuple(), module_data) :: data
            when module_data: %{
                   impls: term(),
                   abst_code: term(),
                   callbacks: term(),
                   specs: [spec_ast()]
                 },
                 data: %{
                   doc_fallback: (() -> ExDoc.DocAST.t()) | nil,
                   extra_annotations: [String.t()],
                   line_override: non_neg_integer() | nil,
                   specs: [spec_ast()]
                 }

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
  @callback type_data(entry :: tuple(), spec :: term()) :: data
            when data: %{
                   spec: spec_ast(),
                   signature_fallback: (() -> String.t()) | nil
                 }

  @doc """
  Formats typespecs.
  """
  @callback typespec(spec :: term(), opts :: keyword()) :: iodata()

  def get(:elixir), do: ExDoc.Language.Elixir
  def get(:erlang), do: ExDoc.Language.Erlang
end
