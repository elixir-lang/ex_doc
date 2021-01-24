defmodule WithModuleDoc do
  @moduledoc "Documentation for `WithModuleDoc`"

  @type a_type() :: any

  @callback a_callback() :: :ok
  @macrocallback a_macrocallback() :: :ok

  def no_doc(), do: :no_doc
  def _no_doc(), do: :no_doc

  @doc false
  def doc_false(), do: :doc_false

  @doc false
  def _doc_false(), do: :doc_false

  @doc "doc..."
  def with_doc(), do: :with_doc

  @doc "doc..."
  def _with_doc(), do: :with_doc
end

defmodule WithoutModuleDoc do
  @type a_type() :: any

  @callback a_callback() :: :ok
  @macrocallback a_macrocallback() :: :ok

  def no_doc(), do: :no_doc
  def _no_doc(), do: :no_doc

  @doc false
  def doc_false(), do: :doc_false

  @doc false
  def _doc_false(), do: :doc_false

  @doc "doc..."
  def with_doc(), do: :with_doc

  @doc "doc..."
  def _with_doc(), do: :with_doc
end
