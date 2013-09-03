defmodule ExDoc.HTMLFormatter.AutolinkTest do
  use ExUnit.Case, async: true

  alias ExDoc.HTMLFormatter.Autolink

  # local_doc

  test "autolink fun/arity in docs" do
    assert Autolink.local_doc("`example/2`", ["example/2"]) == "[`example/2`](#example/2)"
    assert Autolink.local_doc("`example/2` then `example/2`",
      ["example/2"]) == "[`example/2`](#example/2) then [`example/2`](#example/2)"
    assert Autolink.local_doc("`  spaces/0  `", ["spaces/0"]) ==
      "[`  spaces/0  `](#spaces/0)"
    assert Autolink.local_doc("`example/1` and `example/2`",
      ["example/1", "example/2"]) == "[`example/1`](#example/1) and [`example/2`](#example/2)"
    assert Autolink.local_doc("`funny_name\?/1` and `funny_name!/2`",
      ["funny_name\?/1", "funny_name!/2"]) ==
      "[`funny_name\?/1`](#funny_name\?/1) and [`funny_name!/2`](#funny_name!/2)"
  end

  test "autolink doesn't create links for undefined functions in docs" do
    assert Autolink.local_doc("`example/1`", ["example/2"]) == "`example/1`"
    assert Autolink.local_doc("`example/1`", []) == "`example/1`"
  end

  test "autolink doesn't create links for pre-linked functions docs" do
    assert Autolink.local_doc("[`example/1`]()", ["example/1"]) == "[`example/1`]()"
    assert Autolink.local_doc("[the `example/1`]()", ["example/1"]) == "[the `example/1`]()"
  end

  # project_doc

  test "autolink Module.fun/arity in docs" do
    assert Autolink.project_doc("`Mod.example/2`", ["Mod.example/2"]) == 
      "[`Mod.example/2`](Mod.html#example/2)"
    assert Autolink.project_doc("`MyModule.example/2`", ["MyModule.example/2"]) == 
      "[`MyModule.example/2`](MyModule.html#example/2)"
    assert Autolink.project_doc("`MyModule.Nested.example/2`", ["MyModule.Nested.example/2"]) == 
      "[`MyModule.Nested.example/2`](MyModule.Nested.html#example/2)"
    assert Autolink.project_doc("`Mod.example/2` then `Mod.example/2`",
      ["Mod.example/2"]) == "[`Mod.example/2`](Mod.html#example/2) then [`Mod.example/2`](Mod.html#example/2)"
    assert Autolink.project_doc("`  MyModule.spaces/0  `", ["MyModule.spaces/0"]) ==
      "[`  MyModule.spaces/0  `](MyModule.html#spaces/0)"
    assert Autolink.project_doc("`ModA.example/1` and `ModB.example/2`",
      ["ModA.example/1", "ModB.example/2"]) == 
      "[`ModA.example/1`](ModA.html#example/1) and [`ModB.example/2`](ModB.html#example/2)"
    assert Autolink.project_doc("`Mod.funny_name\?/1` and `Mod.funny_name!/2`",
      ["Mod.funny_name\?/1", "Mod.funny_name!/2"]) ==
      "[`Mod.funny_name\?/1`](Mod.html#funny_name\?/1) and [`Mod.funny_name!/2`](Mod.html#funny_name!/2)"
  end

  test "autolink doesn't create links for undefined Mod.functions in docs" do
    assert Autolink.project_doc("`Mod.example/1`", ["Mod.example/2"]) == "`Mod.example/1`"
    assert Autolink.project_doc("`Mod.example/1`", []) == "`Mod.example/1`"
  end

  test "autolink doesn't create links for pre-linked Mod.functions docs" do
    assert Autolink.project_doc("[`Mod.example/1`]()", ["Mod.example/1"]) == "[`Mod.example/1`]()"
    assert Autolink.project_doc("[the `Mod.example/1`]()", ["Mod.example/1"]) == "[the `Mod.example/1`]()"
  end

  # typespec

  test "strip parens in typespecs" do
    assert Autolink.typespec(quote(do: foo({}, bar())), [], []) == %s[foo({}, bar)]
  end

  test "autolink locals in typespecs" do
    assert Autolink.typespec(quote(do: foo(1)), [foo: 1], []) ==
           %s[<a href="#t:foo/1">foo(1)</a>]

    assert Autolink.typespec(quote(do: bar(foo(1))), [foo: 1], []) ==
           %s[bar(<a href="#t:foo/1">foo(1)</a>)]

    assert Autolink.typespec(quote(do: bar(foo(1))), [], []) ==
           %s[bar(foo(1))]
  end

  test "autolink Elixir types in typespecs" do
    assert Autolink.typespec(quote(do: String.t), [], []) ==
           %s[<a href="http://elixir-lang.org/docs/master/String.html#t:t/0">String.t</a>]

    assert Autolink.typespec(quote(do: Unknown.bar()), [], []) ==
           %s[Unknown.bar]
  end

  test "autolink shared aliases in typespecs" do
    assert Autolink.typespec(quote(do: Foo.t), [], [Foo]) ==
           %s[<a href="Foo.html#t:t/0">Foo.t</a>]
  end
end
