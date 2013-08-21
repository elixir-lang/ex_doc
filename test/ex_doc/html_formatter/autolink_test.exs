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

  # all_functions

  test "all_functions returns empty for empty list" do
    assert Autolink.all_functions([]) == []
  end

  test "all_functions returns the correct values" do
    defmodule Outer do
      defmodule Inner do
        def inner_one(_a), do: true
        def inner_two, do: true
      end
      def outer_one, do: true
    end

    modules = ExDoc.Retriever.docs_from_modules([Outer, Outer.Inner], ExDoc.Config.new)
    modules = Autolink.all_functions modules
    set = HashSet.new modules
    expected = HashSet.new ["ExDoc.HTMLFormatter.AutolinkTest.Outer.outer_one/0", "ExDoc.HTMLFormatter.AutolinkTest.Outer.Inner.inner_one/1", "ExDoc.HTMLFormatter.AutolinkTest.Outer.Inner.inner_two/0"]
    assert set == expected
  end

  # typespec

  test "autolink locals in typespecs" do
    assert Autolink.typespec(quote(do: foo(1)), [foo: 1], []) ==
           %b[<a href="#t:foo/1">foo(1)</a>]

    assert Autolink.typespec(quote(do: bar(foo(1))), [foo: 1], []) ==
           %b[bar(<a href="#t:foo/1">foo(1)</a>)]

    assert Autolink.typespec(quote(do: bar(foo(1))), [], []) ==
           %b[bar(foo(1))]
  end

  test "autolink Elixir types in typespecs" do
    assert Autolink.typespec(quote(do: String.t), [], []) ==
           %b[<a href="http://elixir-lang.org/docs/master/String.html#t:t/0">String.t()</a>]

    assert Autolink.typespec(quote(do: Unknown.bar()), [], []) ==
           %b[Unknown.bar()]
  end

  test "autolink shared aliases in typespecs" do
    assert Autolink.typespec(quote(do: Foo.t), [], [Foo]) ==
           %b[<a href="Foo.html#t:t/0">Foo.t()</a>]
  end
end
