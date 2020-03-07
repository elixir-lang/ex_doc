defmodule ExDoc.RefsTest do
  use ExUnit.Case, async: true
  alias ExDoc.Refs

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

    assert Refs.public?({:callback, GenServer, :handle_call, 3})
    refute Refs.public?({:callback, GenServer, :handle_call, 9})

    assert Refs.public?({:function, :lists, :all, 2})
    refute Refs.public?({:function, :lists, :all, 9})

    # TODO: enable when OTP 23.0-rc2 is out (it should have callbacks support)
    # assert Refs.public?({:callback, :gen_server, :handle_call, 3})
    # assert Refs.public?({:callback, :gen_server, :handle_call, 9}) in [true, false]

    assert Refs.public?({:type, :sets, :set, 0})
    assert Refs.public?({:type, :sets, :set, 9}) in [true, false]
  end
end
