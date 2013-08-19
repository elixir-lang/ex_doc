defmodule ExDoc.HTMLFormatter.AutolinkTest do
  use ExUnit.Case, async: true

  alias ExDoc.HTMLFormatter.Autolink

  test "autolink docs fun/arity in module" do
    assert Autolink.docs("`example/2`", ["example/2"]) == "[`example/2`](#example/2)"
    assert Autolink.docs("`example/2` then `example/2`", 
      ["example/2"]) == "[`example/2`](#example/2) then [`example/2`](#example/2)"
    assert Autolink.docs("`  spaces/0  `", ["spaces/0"]) ==
      "[`  spaces/0  `](#spaces/0)"
    assert Autolink.docs("`example/1` and `example/2`", 
      ["example/1", "example/2"]) == "[`example/1`](#example/1) and [`example/2`](#example/2)"
    assert Autolink.docs("`funny_name\?/1` and `funny_name!/2`", 
      ["funny_name\?/1", "funny_name!/2"]) == 
      "[`funny_name\?/1`](#funny_name\?/1) and [`funny_name!/2`](#funny_name!/2)"
  end

  test "autolink docs doesn't create links for undefined functions" do
    assert Autolink.docs("`example/1`", ["example/2"]) == "`example/1`"
    assert Autolink.docs("`example/1`", []) == "`example/1`"
  end

  test "autolink docs doesn't create links for pre-linked functions" do
    assert Autolink.docs("[`example/1`]()", ["example/1"]) == "[`example/1`]()"
    assert Autolink.docs("[the `example/1`]()", ["example/1"]) == "[the `example/1`]()"
  end
end