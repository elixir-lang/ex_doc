defmodule ExDoc.RefsTest do
  use ExUnit.Case, async: true
  alias ExDoc.Refs

  test "get_visibility/1" do
    assert Refs.get_visibility({:module, String}) == :public
    assert Refs.get_visibility({:module, String.Unicode}) == :hidden
    assert Refs.get_visibility({:module, Unknown}) == :undefined

    assert Refs.get_visibility({:function, Enum, :join, 1}) == :public
    assert Refs.get_visibility({:function, Enum, :join, 2}) == :public
    assert Refs.get_visibility({:function, String.Unicode, :version, 0}) == :hidden
    assert Refs.get_visibility({:function, String.Unicode, :version, 9}) == :undefined
    assert Refs.get_visibility({:function, Enum, :join, 9}) == :undefined

    assert Refs.get_visibility({:function, Unknown, :unknown, 0}) == :undefined

    # macros are classified as functions
    assert Refs.get_visibility({:function, Kernel, :def, 2}) == :public

    assert Refs.get_visibility({:type, String, :t, 0}) == :public
    assert Refs.get_visibility({:type, String, :t, 1}) == :undefined
    assert Refs.get_visibility({:type, Module, :definition, 0}) == :private

    assert Refs.get_visibility({:callback, GenServer, :handle_call, 3}) == :public
    assert Refs.get_visibility({:callback, GenServer, :handle_call, 9}) == :undefined

    assert Refs.get_visibility({:function, :lists, :all, 2}) == :public
    assert Refs.get_visibility({:function, :lists, :all, 9}) == :undefined

    assert Refs.get_visibility({:type, :sets, :set, 0}) == :public
    assert Refs.get_visibility({:type, :sets, :set, 9}) == :public
  end

  test "public?/1" do
    assert Refs.public?({:module, String})
    refute Refs.public?({:module, String.Unicode})
    refute Refs.public?({:module, Unknown})

    assert Refs.public?({:function, Enum, :join, 1})
    assert Refs.public?({:function, Enum, :join, 2})
    refute Refs.public?({:function, Enum, :join, 9})

    refute Refs.public?({:function, Unknown, :unknown, 0})

    # macros are classified as functions
    assert Refs.public?({:function, Kernel, :def, 2})

    assert Refs.public?({:type, String, :t, 0})
    refute Refs.public?({:type, String, :t, 1})
    refute Refs.public?({:type, Module, :definition, 0})

    assert Refs.public?({:callback, GenServer, :handle_call, 3})
    refute Refs.public?({:callback, GenServer, :handle_call, 9})

    assert Refs.public?({:function, :lists, :all, 2})
    refute Refs.public?({:function, :lists, :all, 9})

    if opt_release() >= 23 do
      assert Refs.public?({:callback, :gen_server, :handle_call, 3})
      assert Refs.public?({:callback, :gen_server, :handle_call, 9}) in [true, false]
    end

    assert Refs.public?({:type, :sets, :set, 0})
    assert Refs.public?({:type, :sets, :set, 9}) in [true, false]
  end

  test "from_chunk/2 with module that doesn't exist" do
    result = ExDoc.Utils.Code.fetch_docs(:elixir)
    assert {:none, _} = ExDoc.Refs.from_chunk(Elixir, result)
  end

  defp opt_release() do
    System.otp_release()
    |> String.to_integer()
  end
end
