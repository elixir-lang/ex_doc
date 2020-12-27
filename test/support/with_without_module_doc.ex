defmodule WithModuleDoc do
  @moduledoc """
  - `t:t/0`, `t:WithModuleDoc.t/0`
  - `c:handle_foo/0`, `c:WithModuleDoc.handle_foo/0`
  - `c:handle_macro_foo/0`, `c:WithModuleDoc.handle_macro_foo/0`
  - `foo/1`, `WithModuleDoc.foo/1`
  - `_foo/1`, `WithModuleDoc._foo/1`
  - `_foo/2`, `WithModuleDoc._foo/2`
  - `foo/2`, `WithModuleDoc.foo/2`
  - `foo/3`, `WithModuleDoc.foo/3`
  - `_foo/3`, `WithModuleDoc._foo/3`

  - `t:t/0`, `t:WithoutModuleDoc.t/0`
  - `c:handle_foo/0`, `c:WithoutModuleDoc.handle_foo/0`
  - `c:handle_macro_foo/0`, `c:WithoutModuleDoc.handle_macro_foo/0`
  - `foo/1`, `WithoutModuleDoc.foo/1`
  - `_foo/1`, `WithoutModuleDoc._foo/1`
  - `_foo/2`, `WithoutModuleDoc._foo/2`
  - `foo/2`, `WithoutModuleDoc.foo/2`
  - `foo/3`, `WithoutModuleDoc.foo/3`
  - `_foo/3`, `WithoutModuleDoc._foo/3`

  """

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
