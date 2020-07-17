defmodule ExDoc.RefsTest do
  use ExUnit.Case, async: true
  alias ExDoc.Refs

  test "get_visibility/1" do
    assert Refs.get_visibility({:module, String}) == :public
    assert Refs.get_visibility({:module, String.Unicode}) == :hidden
    assert Refs.get_visibility({:module, Unknown}) == :undefined

    assert Refs.get_visibility({:function, RefsTest.Public, :function_public, 1}) == :public
    assert Refs.get_visibility({:function, RefsTest.Public, :function_private, 1}) == :private
    assert Refs.get_visibility({:function, RefsTest.Public, :macro_public, 1}) == :public
    assert Refs.get_visibility({:function, RefsTest.Public, :macro_private, 1}) == :private
    assert Refs.get_visibility({:function, RefsTest.Public, :function_public, 2}) == :undefined
    assert Refs.get_visibility({:function, RefsTest.Public, :does_not_exist, 1}) == :undefined

    assert Refs.get_visibility({:function, RefsTest.Hidden, :function_public, 1}) == :hidden
    assert Refs.get_visibility({:function, RefsTest.Hidden, :function_private, 1}) == :private
    assert Refs.get_visibility({:function, RefsTest.Hidden, :macro_public, 1}) == :hidden
    assert Refs.get_visibility({:function, RefsTest.Hidden, :macro_private, 1}) == :private
    assert Refs.get_visibility({:function, RefsTest.Hidden, :function_public, 2}) == :undefined
    assert Refs.get_visibility({:function, RefsTest.Hidden, :does_not_exist, 1}) == :undefined

    assert Refs.get_visibility({:function, Unknown, :unknown, 0}) == :undefined

    # macros are classified as functions
    assert Refs.get_visibility({:function, Kernel, :def, 2}) == :public

    assert Refs.get_visibility({:type, String, :t, 0}) == :public
    assert Refs.get_visibility({:type, String, :t, 10}) == :undefined

    assert Refs.get_visibility({:type, RefsTest.Public, :a_type, 0}) == :public
    assert Refs.get_visibility({:type, RefsTest.Public, :a_typep, 0}) == :private
    assert Refs.get_visibility({:type, RefsTest.Public, :an_opaque, 0}) == :public

    assert Refs.get_visibility({:type, RefsTest.Hidden, :a_type, 0}) == :hidden
    assert Refs.get_visibility({:type, RefsTest.Hidden, :a_typep, 0}) == :private
    assert Refs.get_visibility({:type, RefsTest.Hidden, :an_opaque, 0}) == :hidden

    assert Refs.get_visibility({:callback, GenServer, :handle_call, 3}) == :public
    assert Refs.get_visibility({:callback, GenServer, :handle_call, 9}) == :undefined
    assert Refs.get_visibility({:callback, Mix.RemoteConverger, :converge, 2}) == :hidden
    assert Refs.get_visibility({:callback, RefsTest.Public, :a_callback, 1}) == :public
    assert Refs.get_visibility({:callback, RefsTest.Public, :a_macrocallback, 1}) == :public
    assert Refs.get_visibility({:callback, RefsTest.Hidden, :a_callback, 1}) == :hidden
    assert Refs.get_visibility({:callback, RefsTest.Hidden, :a_macrocallback, 1}) == :hidden

    assert Refs.get_visibility({:function, :lists, :all, 2}) == :public
    assert Refs.get_visibility({:function, :lists, :all, 9}) == :undefined
    assert Refs.get_visibility({:function, :lists, :mergel, 2}) == :private

    assert Refs.get_visibility({:type, :sets, :set, 0}) == :public
    assert Refs.get_visibility({:type, :sets, :set, 9}) == :public
  end

  test "public?/1" do
    assert Refs.public?({:module, String})
    refute Refs.public?({:module, String.Unicode})
    refute Refs.public?({:module, Unknown})

    if opt_release() >= 23 do
      assert Refs.public?({:callback, :gen_server, :handle_call, 3})
      assert Refs.public?({:callback, :gen_server, :handle_call, 9}) in [true, false]
    end
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
