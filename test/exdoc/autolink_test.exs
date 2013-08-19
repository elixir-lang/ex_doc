defmodule ExDoc.AutolinkTest do
  use ExUnit.Case, async: true

  alias ExDoc.Autolink

  test "autolink fun/arity in module" do
    assert Autolink.locals("`example/2`", ["example/2"]) == "[`example/2`](#example/2)"
    assert Autolink.locals("`example/2` then `example/2`", 
      ["example/2"]) == "[`example/2`](#example/2) then [`example/2`](#example/2)"
    assert Autolink.locals("`  spaces/0  `", ["spaces/0"]) ==
      "[`  spaces/0  `](#spaces/0)"
    assert Autolink.locals("`example/1` and `example/2`", 
      ["example/1", "example/2"]) == "[`example/1`](#example/1) and [`example/2`](#example/2)"
    assert Autolink.locals("`funny_name\?/1` and `funny_name!/2`", 
      ["funny_name\?/1", "funny_name!/2"]) == 
      "[`funny_name\?/1`](#funny_name\?/1) and [`funny_name!/2`](#funny_name!/2)"
  end

  test "autolink doesn't create links for undefined functions" do
    assert Autolink.locals("`example/1`", ["example/2"]) == "`example/1`"
    assert Autolink.locals("`example/1`", []) == "`example/1`"
  end

  test "autolink doesn't create links for pre-linked functions" do
    assert Autolink.locals("[`example/1`]()", ["example/1"]) == "[`example/1`]()"
    assert Autolink.locals("[the `example/1`]()", ["example/1"]) == "[the `example/1`]()"
  end
end