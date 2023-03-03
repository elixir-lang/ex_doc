defmodule ExDoc.RefsTest do
  use ExUnit.Case, async: true
  alias ExDoc.Refs

  defmodule InMemory do
    @type a_type() :: any

    @callback a_callback() :: :ok
    @macrocallback a_macrocallback() :: :ok

    def no_doc(), do: :no_doc

    @doc false
    def doc_false(), do: :doc_false

    @doc "doc..."
    def with_doc(), do: :with_doc
  end

  test "get_visibility/1" do
    assert Refs.get_visibility({:module, Code}) == :public
    assert Refs.get_visibility({:module, Code.Typespec}) == :hidden
    assert Refs.get_visibility({:module, Unknown}) == :undefined
    assert Refs.get_visibility({:module, InMemory}) == :limited

    assert Refs.get_visibility({:function, Enum, :join, 1}) == :public
    assert Refs.get_visibility({:function, Enum, :join, 2}) == :public
    assert Refs.get_visibility({:function, Code.Typespec, :spec_to_quoted, 2}) == :hidden
    assert Refs.get_visibility({:function, Code.Typespec, :spec_to_quoted, 9}) == :undefined
    assert Refs.get_visibility({:function, Enum, :join, 9}) == :undefined
    assert Refs.get_visibility({:function, Enum, :_join, 9}) == :undefined
    assert Refs.get_visibility({:function, :lists, :all, 2}) == :public
    assert Refs.get_visibility({:function, :lists, :all, 9}) == :undefined
    assert Refs.get_visibility({:function, :lists, :_all, 9}) == :undefined
    assert Refs.get_visibility({:function, InMemory, :with_doc, 0}) == :public
    assert Refs.get_visibility({:function, InMemory, :non_existant, 0}) == :undefined
    assert Refs.get_visibility({:function, WithModuleDoc, :no_doc, 0}) == :public
    assert Refs.get_visibility({:function, WithModuleDoc, :_no_doc, 0}) == :hidden
    assert Refs.get_visibility({:function, WithModuleDoc, :_doc_false, 0}) == :hidden

    # unable to read documentation, visibility is set to :public
    assert Refs.get_visibility({:function, InMemory, :no_doc, 0}) == :public
    assert Refs.get_visibility({:function, InMemory, :doc_false, 0}) == :public
    assert Refs.get_visibility({:function, InMemory, :with_doc, 0}) == :public
    assert Refs.get_visibility({:function, InMemory, :non_existant, 0}) == :undefined

    # macros are classified as functions
    assert Refs.get_visibility({:function, Kernel, :def, 2}) == :public
    assert Refs.get_visibility({:function, Unknown, :unknown, 0}) == :undefined

    assert Refs.get_visibility({:type, String, :t, 0}) == :public
    assert Refs.get_visibility({:type, String, :t, 1}) == :undefined
    assert Refs.get_visibility({:type, :sets, :set, 0}) == :public
    assert Refs.get_visibility({:type, :sets, :set, 9}) == :undefined
    assert Refs.get_visibility({:type, WithoutModuleDoc, :a_type, 0}) == :public

    # types cannot be read for inmemory modules
    assert Refs.get_visibility({:type, InMemory, :a_type, 0}) == :undefined

    assert Refs.get_visibility({:callback, GenServer, :handle_call, 3}) == :public
    assert Refs.get_visibility({:callback, GenServer, :handle_call, 9}) == :undefined
    assert Refs.get_visibility({:callback, :gen_server, :handle_call, 3}) == :public
    assert Refs.get_visibility({:callback, :gen_server, :handle_call, 9}) == :undefined
    assert Refs.get_visibility({:callback, InMemory, :a_callback, 0}) == :public
    assert Refs.get_visibility({:callback, WithModuleDoc, :a_macrocallback, 0}) == :public
    assert Refs.get_visibility({:callback, WithoutModuleDoc, :a_callback, 0}) == :public
    assert Refs.get_visibility({:callback, WithoutModuleDoc, :a_macrocallback, 0}) == :public
    assert Refs.get_visibility({:callback, InMemory, :a_callback, 9}) == :undefined
  end

  test "insert_from_chunk/2 with module that doesn't exist" do
    result = Code.fetch_docs(:elixir)
    assert :ok = ExDoc.Refs.insert_from_chunk(Elixir, result)
  end
end
