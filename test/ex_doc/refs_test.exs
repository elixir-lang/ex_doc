defmodule ExDoc.RefsTest do
  use ExUnit.Case, async: true
  alias ExDoc.Refs

  defmodule InMemory do
    @type t() :: :ok

    @callback handle_foo() :: :ok

    def foo(), do: :ok
  end

  test "get_visibility/1" do
    assert Refs.get_visibility({:module, String}) == :public
    assert Refs.get_visibility({:module, String.Unicode}) == :hidden
    assert Refs.get_visibility({:module, Unknown}) == :undefined
    assert Refs.get_visibility({:module, InMemory}) == :public

    assert Refs.get_visibility({:function, Enum, :join, 1}) == :public
    assert Refs.get_visibility({:function, Enum, :join, 2}) == :public
    assert Refs.get_visibility({:function, String.Unicode, :version, 0}) == :hidden
    assert Refs.get_visibility({:function, String.Unicode, :version, 9}) == :undefined
    assert Refs.get_visibility({:function, Enum, :join, 9}) == :undefined
    assert Refs.get_visibility({:function, :lists, :all, 2}) == :public
    assert Refs.get_visibility({:function, :lists, :all, 9}) == :undefined
    assert Refs.get_visibility({:function, InMemory, :foo, 0}) == :public
    assert Refs.get_visibility({:function, InMemory, :foo, 9}) == :undefined

    # macros are classified as functions
    assert Refs.get_visibility({:function, Kernel, :def, 2}) == :public

    assert Refs.get_visibility({:function, Unknown, :unknown, 0}) == :undefined

    assert Refs.get_visibility({:type, String, :t, 0}) == :public
    assert Refs.get_visibility({:type, String, :t, 1}) == :undefined
    assert Refs.get_visibility({:type, :sets, :set, 0}) == :public
    assert Refs.get_visibility({:type, :sets, :set, 9}) == :undefined

    # types are in abstract_code chunk so not available for in-memory modules
    assert Refs.get_visibility({:type, InMemory, :t, 0}) == :undefined

    # @typep
    assert Refs.get_visibility({:type, :sets, :seg, 0}) == :undefined

    assert Refs.get_visibility({:callback, GenServer, :handle_call, 3}) == :public
    assert Refs.get_visibility({:callback, GenServer, :handle_call, 9}) == :undefined
    assert Refs.get_visibility({:callback, :gen_server, :handle_call, 3}) == :public
    assert Refs.get_visibility({:callback, :gen_server, :handle_call, 9}) == :undefined
    assert Refs.get_visibility({:callback, InMemory, :handle_foo, 0}) == :public
    assert Refs.get_visibility({:callback, InMemory, :handle_foo, 9}) == :undefined
  end

  test "public?/1" do
    assert Refs.public?({:module, String})
    refute Refs.public?({:module, String.Unicode})
  end

  test "insert_from_chunk/2 with module that doesn't exist" do
    result = ExDoc.Utils.Code.fetch_docs(:elixir)
    assert :ok = ExDoc.Refs.insert_from_chunk(Elixir, result)
  end
end
