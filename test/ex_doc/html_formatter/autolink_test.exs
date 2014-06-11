defmodule ExDoc.Formatter.HTML.AutolinkTest do
  use ExUnit.Case, async: true

  alias ExDoc.Formatter.HTML.Autolink

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

  test "autolink creates links for special forms" do
    assert Autolink.local_doc("`++/2`", ["++/2"]) === "[`++/2`](#++/2)"
    assert Autolink.local_doc("`!/1`", ["!/1"]) === "[`!/1`](#!/1)"
    assert Autolink.local_doc("`../2`", ["../2"]) === "[`../2`](#../2)"
    assert Autolink.local_doc("`--/2`", ["--/2"]) === "[`--/2`](#--/2)"
  end

  # project_functions

  test "autolink Module.fun/arity in docs" do
    assert Autolink.project_functions("`Mod.example/2`", ["Mod.example/2"]) ==
      "[`Mod.example/2`](Mod.html#example/2)"
    assert Autolink.project_functions("`MyModule.example/2`", ["MyModule.example/2"]) ==
      "[`MyModule.example/2`](MyModule.html#example/2)"
    assert Autolink.project_functions("`MyModule.Nested.example/2`", ["MyModule.Nested.example/2"]) ==
      "[`MyModule.Nested.example/2`](MyModule.Nested.html#example/2)"
    assert Autolink.project_functions("`Mod.example/2` then `Mod.example/2`",
      ["Mod.example/2"]) == "[`Mod.example/2`](Mod.html#example/2) then [`Mod.example/2`](Mod.html#example/2)"
    assert Autolink.project_functions("`  MyModule.spaces/0  `", ["MyModule.spaces/0"]) ==
      "[`  MyModule.spaces/0  `](MyModule.html#spaces/0)"
    assert Autolink.project_functions("`ModA.example/1` and `ModB.example/2`",
      ["ModA.example/1", "ModB.example/2"]) ==
      "[`ModA.example/1`](ModA.html#example/1) and [`ModB.example/2`](ModB.html#example/2)"
    assert Autolink.project_functions("`Mod.funny_name\?/1` and `Mod.funny_name!/2`",
      ["Mod.funny_name\?/1", "Mod.funny_name!/2"]) ==
      "[`Mod.funny_name\?/1`](Mod.html#funny_name\?/1) and [`Mod.funny_name!/2`](Mod.html#funny_name!/2)"
  end

  test "autolink creates links for Module special forms" do
    assert Autolink.project_functions("`Mod.++/2`", ["Mod.++/2"]) === "[`Mod.++/2`](Mod.html#++/2)"
    assert Autolink.project_functions("`Mod.!/1`", ["Mod.!/1"]) === "[`Mod.!/1`](Mod.html#!/1)"
    assert Autolink.project_functions("`Mod.../2`", ["Mod.../2"]) === "[`Mod.../2`](Mod.html#../2)"
    assert Autolink.project_functions("`Mod.--/2`", ["Mod.--/2"]) === "[`Mod.--/2`](Mod.html#--/2)"
  end

  test "autolink doesn't create links for undefined Mod.functions in docs" do
    assert Autolink.project_functions("`Mod.example/1`", ["Mod.example/2"]) == "`Mod.example/1`"
    assert Autolink.project_functions("`Mod.example/1`", []) == "`Mod.example/1`"
  end

  test "autolink doesn't create links for pre-linked Mod.functions docs" do
    assert Autolink.project_functions("[`Mod.example/1`]()", ["Mod.example/1"]) == "[`Mod.example/1`]()"
    assert Autolink.project_functions("[the `Mod.example/1`]()", ["Mod.example/1"]) == "[the `Mod.example/1`]()"
  end

  test "autolink Modules in docs" do
    assert Autolink.project_modules("`MyModule`", ["MyModule"]) == "[`MyModule`](MyModule.html)"
    assert Autolink.project_modules("`MyModule.Nested`", ["MyModule.Nested"]) == "[`MyModule.Nested`](MyModule.Nested.html)"
    assert Autolink.project_modules("`MyModule.Nested.Deep`", ["MyModule.Nested.Deep"]) ==
      "[`MyModule.Nested.Deep`](MyModule.Nested.Deep.html)"
  end

  test "autolink Modules doesn't link functions" do
    assert Autolink.project_modules("`Mod.example/1`", ["Mod"]) == "`Mod.example/1`"
  end

  test "autolink doesn't create links for pre-linked Mod docs" do
    assert Autolink.project_functions("[`Mod`](other.html)", ["Mod"]) == "[`Mod`](other.html)"
    assert Autolink.project_functions("[the `Mod`](other.html)", ["Mod"]) == "[the `Mod`](other.html)"
    assert Autolink.project_functions("[the `Mod.Nested`](other.html)", ["Mod.Nested"]) == "[the `Mod.Nested`](other.html)"
  end

  test "autolink doesn't create links for undefined modules"do
    assert Autolink.project_modules("`MyModule`", []) == "`MyModule`"
    assert Autolink.project_modules("`MyModule`", ["DiffModule"]) == "`MyModule`"
    assert Autolink.project_modules("`MyModule.Nested`", ["MyModule.DiffNested"]) == "`MyModule.Nested`"
  end

  # erlang functions

  @erlang_docs "http://www.erlang.org/doc/man/"

  test "autolink to erlang functions" do
    assert Autolink.erlang_functions("`:erlang.apply/2`") == "[`:erlang.apply/2`](#{@erlang_docs}erlang.html#apply-2)"
    assert Autolink.erlang_functions("`:erlang.adler32/2`") == "[`:erlang.adler32/2`](#{@erlang_docs}erlang.html#adler32-2)"
    assert Autolink.erlang_functions("`:erlang.apply/2` `:erlang.apply/3`") ==
      "[`:erlang.apply/2`](#{@erlang_docs}erlang.html#apply-2) [`:erlang.apply/3`](#{@erlang_docs}erlang.html#apply-3)"

    assert Autolink.erlang_functions("`:erl_prim_loader.get_file/1`") ==
      "[`:erl_prim_loader.get_file/1`](#{@erlang_docs}erl_prim_loader.html#get_file-1)"
    assert Autolink.erlang_functions("`:zlib.deflateInit/2`") == "[`:zlib.deflateInit/2`](#{@erlang_docs}zlib.html#deflateInit-2)"
  end

  test "autolink erlang doesn't create links for pre-linked docs" do
    assert Autolink.erlang_functions("[`:erlang.apply/2`](other.html)") == "[`:erlang.apply/2`](other.html)"
    assert Autolink.erlang_functions("[the `:erlang.apply/2`](other.html)") == "[the `:erlang.apply/2`](other.html)"

    assert Autolink.erlang_functions("`:erlang`") == "`:erlang`"
  end

  test "autolink erlang doesn't create links for functions that aren't part of the erlang distribution" do
    assert Autolink.erlang_functions("`:erl_prim_loader.adder32_combine/0`") ==
      "`:erl_prim_loader.adder32_combine/0`"
    assert Autolink.erlang_functions("`:erl_prim_loader_non.adder32_combine/0`") ==
      "`:erl_prim_loader_non.adder32_combine/0`"
  end

  # typespec

  test "strip parens in typespecs" do
    assert Autolink.typespec(quote(do: foo({}, bar())), [], []) == ~s[foo({}, bar)]
  end

  test "autolink locals in typespecs" do
    assert Autolink.typespec(quote(do: foo(1)), [foo: 1], []) ==
           ~s[<a href="#t:foo/1">foo(1)</a>]

    assert Autolink.typespec(quote(do: bar(foo(1))), [foo: 1], []) ==
           ~s[bar(<a href="#t:foo/1">foo(1)</a>)]

    assert Autolink.typespec(quote(do: bar(foo(1))), [], []) ==
           ~s[bar(foo(1))]
  end

  test "autolink Elixir types in typespecs" do
    assert Autolink.typespec(quote(do: String.t), [], []) ==
           ~s[<a href="http://elixir-lang.org/docs/stable/elixir/String.html#t:t/0">String.t</a>]

    assert Autolink.typespec(quote(do: Unknown.bar()), [], []) ==
           ~s[Unknown.bar]
  end

  test "autolink shared aliases in typespecs" do
    assert Autolink.typespec(quote(do: Foo.t), [], [Foo]) ==
           ~s[<a href="Foo.html#t:t/0">Foo.t</a>]
  end
end
