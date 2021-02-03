defmodule ExDoc.Language do
  @moduledoc false

  @callback filter_prefix_pattern(String.t() | nil) :: String.t()

  @callback module_type(module()) :: atom()

  @callback skip_module?(module()) :: boolean()

  @callback skip_module_type?(module_type :: atom()) :: boolean()

  @callback actual_def(kind :: atom(), name :: atom(), arity) :: {name :: atom(), arity()}

  @callback extra_annotations(kind :: atom(), name :: atom(), arity) :: [String.t()]

  @callback normalize_specs(specs :: [term()], kind :: atom(), name :: atom(), arity) :: [term()]

  @callback module_title_and_id(module(), module_type :: atom()) ::
              {title :: String.t(), id :: String.t()}

  @callback doc_fallback(name :: atom(), arity(), impl :: term(), metadata :: term()) ::
              ExDoc.DocAST.t() | nil

  def get(:elixir), do: ExDoc.Language.Elixir
  def get(:erlang), do: ExDoc.Language.Erlang
end
