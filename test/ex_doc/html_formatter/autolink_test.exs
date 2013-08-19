defmodule ExDoc.HTMLFormatter.AutolinkTest do
  use ExUnit.Case, async: true

  alias ExDoc.HTMLFormatter.Autolink

  # doc

  test "autolink fun/arity in docs" do
    assert Autolink.doc("`example/2`", ["example/2"]) == "[`example/2`](#example/2)"
    assert Autolink.doc("`example/2` then `example/2`",
      ["example/2"]) == "[`example/2`](#example/2) then [`example/2`](#example/2)"
    assert Autolink.doc("`  spaces/0  `", ["spaces/0"]) ==
      "[`  spaces/0  `](#spaces/0)"
    assert Autolink.doc("`example/1` and `example/2`",
      ["example/1", "example/2"]) == "[`example/1`](#example/1) and [`example/2`](#example/2)"
    assert Autolink.doc("`funny_name\?/1` and `funny_name!/2`",
      ["funny_name\?/1", "funny_name!/2"]) ==
      "[`funny_name\?/1`](#funny_name\?/1) and [`funny_name!/2`](#funny_name!/2)"
  end

  test "autolink doesn't create links for undefined functions in docs" do
    assert Autolink.doc("`example/1`", ["example/2"]) == "`example/1`"
    assert Autolink.doc("`example/1`", []) == "`example/1`"
  end

  test "autolink doesn't create links for pre-linked functions docs" do
    assert Autolink.doc("[`example/1`]()", ["example/1"]) == "[`example/1`]()"
    assert Autolink.doc("[the `example/1`]()", ["example/1"]) == "[the `example/1`]()"
  end

  # typespec

  test "autolink locals" do
    assert Autolink.typespec(quote(do: foo(1)), [foo: 1]) ==
           %b[<a href="#t:foo/1">foo(1)</a>]

    assert Autolink.typespec(quote(do: bar(foo(1))), [foo: 1]) ==
           %b[bar(<a href="#t:foo/1">foo(1)</a>)]

    assert Autolink.typespec(quote(do: bar(foo(1))), []) ==
           %b[bar(foo(1))]
  end
end