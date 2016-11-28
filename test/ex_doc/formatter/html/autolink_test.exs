defmodule ExDoc.Formatter.HTML.AutolinkTest do
  use ExUnit.Case, async: true

  alias ExDoc.Formatter.HTML.Autolink

  @elixir_docs "https://hexdocs.pm/"
  @erlang_docs "http://www.erlang.org/doc/man/"

  # local_doc

  test "autolink fun/arity in docs" do
    assert Autolink.local_doc("`example/2`", ["example/2"]) == "[`example/2`](#example/2)"
    assert Autolink.local_doc("`__ENV__/0`", ["__ENV__/0"]) == "[`__ENV__/0`](#__ENV__/0)"
    assert Autolink.local_doc("`example/2` then `example/2`",
      ["example/2"]) == "[`example/2`](#example/2) then [`example/2`](#example/2)"
    assert Autolink.local_doc("`  spaces/0  `", ["spaces/0"]) ==
      "[`spaces/0`](#spaces/0)"
    assert Autolink.local_doc("`example/1` and `example/2`",
      ["example/1", "example/2"]) == "[`example/1`](#example/1) and [`example/2`](#example/2)"
    assert Autolink.local_doc("`funny_name\?/1` and `funny_name!/2`",
      ["funny_name\?/1", "funny_name!/2"]) ==
      "[`funny_name\?/1`](#funny_name\?/1) and [`funny_name!/2`](#funny_name!/2)"
  end

  test "autolink to local callbacks" do
    # `split_function` must also return locally defined callbacks
    # format should be: "c:<fun>/<arity>"
    assert Autolink.local_doc("`c:fun/2`", ["c:fun/2"]) == "[`fun/2`](#c:fun/2)"
  end

  test "autolink to local types" do
    assert Autolink.local_doc("`t:my_type/0`", ["t:my_type/0"]) == "[`my_type/0`](#t:my_type/0)"
    assert Autolink.local_doc("`t:my_type/1`", ["t:my_type/1"]) == "[`my_type/1`](#t:my_type/1)"
    # links to types without arity don't work
    assert Autolink.local_doc("`t:my_type`", ["t:my_type/0"]) == "`t:my_type`"
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
    assert Autolink.local_doc("`<<>>/1`", ["<<>>/1"]) === "[`<<>>/1`](#%3C%3C%3E%3E/1)"
    assert Autolink.local_doc("`{}/1`", ["{}/1"]) === "[`{}/1`](#%7B%7D/1)"
  end

  # elixir_functions

  test "autolink functions Module.fun/arity in docs" do
    assert Autolink.elixir_functions("`Mod.example/2`", ["Mod.example/2"]) ==
      "[`Mod.example/2`](Mod.html#example/2)"
    assert Autolink.elixir_functions("`Mod.__ENV__/2`", ["Mod.__ENV__/2"]) ==
      "[`Mod.__ENV__/2`](Mod.html#__ENV__/2)"
    assert Autolink.elixir_functions("`MyModule.example/2`", ["MyModule.example/2"]) ==
      "[`MyModule.example/2`](MyModule.html#example/2)"
    assert Autolink.elixir_functions("`MyModule.Nested.example/2`", ["MyModule.Nested.example/2"]) ==
      "[`MyModule.Nested.example/2`](MyModule.Nested.html#example/2)"
    assert Autolink.elixir_functions("`Mod.example/2` then `Mod.example/2`",
      ["Mod.example/2"]) == "[`Mod.example/2`](Mod.html#example/2) then [`Mod.example/2`](Mod.html#example/2)"
    assert Autolink.elixir_functions("`  MyModule.spaces/0  `", ["MyModule.spaces/0"]) ==
      "[`MyModule.spaces/0`](MyModule.html#spaces/0)"
    assert Autolink.elixir_functions("`ModA.example/1` and `ModB.example/2`",
      ["ModA.example/1", "ModB.example/2"]) ==
      "[`ModA.example/1`](ModA.html#example/1) and [`ModB.example/2`](ModB.html#example/2)"
    assert Autolink.elixir_functions("`Mod.funny_name\?/1` and `Mod.funny_name!/2`",
      ["Mod.funny_name\?/1", "Mod.funny_name!/2"]) ==
      "[`Mod.funny_name\?/1`](Mod.html#funny_name\?/1) and [`Mod.funny_name!/2`](Mod.html#funny_name!/2)"
  end

  test "autolink functions Module.fun/arity in elixir" do
    assert Autolink.elixir_functions("`String.upcase/1`", ["Mod.example/2"]) ==
           "[`String.upcase/1`](#{@elixir_docs}elixir/String.html#upcase/1)"
    assert Autolink.elixir_functions("`Mix.env/0`", ["Mod.example/2"]) ==
           "[`Mix.env/0`](#{@elixir_docs}mix/Mix.html#env/0)"
  end

  test "autolink functions creates links for modules special forms" do
    assert Autolink.elixir_functions("`Mod.++/2`", ["Mod.++/2"]) === "[`Mod.++/2`](Mod.html#++/2)"
    assert Autolink.elixir_functions("`Mod.!/1`", ["Mod.!/1"]) === "[`Mod.!/1`](Mod.html#!/1)"
    assert Autolink.elixir_functions("`Mod.../2`", ["Mod.../2"]) === "[`Mod.../2`](Mod.html#../2)"
    assert Autolink.elixir_functions("`Mod.--/2`", ["Mod.--/2"]) === "[`Mod.--/2`](Mod.html#--/2)"
    assert Autolink.elixir_functions("`Mod.%/2`", ["Mod.%/2"]) === "[`Mod.%/2`](Mod.html#%25/2)"
    assert Autolink.elixir_functions("`Mod.<<>>/1`", ["Mod.<<>>/1"]) === "[`Mod.<<>>/1`](Mod.html#%3C%3C%3E%3E/1)"
  end

  test "autolink functions creates links for callbacks" do
    assert Autolink.elixir_functions("`c:Mod.++/2`", ["c:Mod.++/2"]) ===
           "[`Mod.++/2`](Mod.html#c:++/2)"
  end

  test "autolink functions doesn't create links for undefined Mod.functions in docs" do
    assert Autolink.elixir_functions("`Mod.example/1`", ["Mod.example/2"]) == "`Mod.example/1`"
    assert Autolink.elixir_functions("`Mod.example/1`", []) == "`Mod.example/1`"
  end

  test "autolink functions doesn't create links for pre-linked Mod.functions docs" do
    assert Autolink.elixir_functions("[`Mod.example/1`]()", ["Mod.example/1"]) == "[`Mod.example/1`]()"
    assert Autolink.elixir_functions("[the `Mod.example/1`]()", ["Mod.example/1"]) == "[the `Mod.example/1`]()"
  end

  test "autolink functions to types in the project" do
    # use the same approach for elixir_functions as for local_docs
    assert Autolink.elixir_functions("`t:MyModule.my_type/0`",
      ["t:MyModule.my_type/0"]) ==  "[`MyModule.my_type/0`](MyModule.html#t:my_type/0)"
    assert Autolink.elixir_functions("`t:MyModule.my_type`",
      ["t:MyModule.my_type/0"]) ==  "`t:MyModule.my_type`"
  end

  # elixir_modules

  test "autolink modules in docs" do
    assert Autolink.elixir_modules("`MyModule`", ["MyModule"], "MyModule") == "[`MyModule`](MyModule.html#content)"
    assert Autolink.elixir_modules("`MyModule.Nested`", ["MyModule.Nested"], "MyModule.Nested") == "[`MyModule.Nested`](MyModule.Nested.html#content)"
    assert Autolink.elixir_modules("`MyModule.Nested.Deep`", ["MyModule.Nested.Deep"], "MyModule.Nested.Deep") ==
      "[`MyModule.Nested.Deep`](MyModule.Nested.Deep.html#content)"
  end

  test "autolink modules in elixir" do
    assert Autolink.elixir_modules("`String`", ["MyModule"], "MyModule") ==
           "[`String`](#{@elixir_docs}elixir/String.html)"
    assert Autolink.elixir_modules("`Mix`", ["MyModule"], "MyModule") ==
           "[`Mix`](#{@elixir_docs}mix/Mix.html)"
  end

  test "autolink dependencies modules" do
    lib_dirs = [{Application.app_dir(:earmark), "https://hexdocs.pm/earmark/"}]
    assert Autolink.elixir_modules("`Earmark`", ["MyModule"], "MyModule", ".html", lib_dirs) ==
           "[`Earmark`](https://hexdocs.pm/earmark/Earmark.html)"
  end

  test "autolink modules doesn't link functions" do
    assert Autolink.elixir_modules("`Mod.example/1`", ["Mod"], "Mod") == "`Mod.example/1`"
  end

  test "autolink modules doesn't create links for undefined modules"do
    assert Autolink.elixir_modules("`MyModule`", []) == "`MyModule`"
    assert Autolink.elixir_modules("`MyModule`", ["DiffModule"]) == "`MyModule`"
    assert Autolink.elixir_modules("`MyModule.Nested`", ["MyModule.DiffNested"]) == "`MyModule.Nested`"
  end

  test "autolink modules doesn't create links for pre-linked Mod docs" do
    assert Autolink.elixir_modules("[`Mod`](other.html)", ["Mod"]) == "[`Mod`](other.html)"
    assert Autolink.elixir_modules("[the `Mod`](other.html)", ["Mod"]) == "[the `Mod`](other.html)"
    assert Autolink.elixir_modules("[the `Mod.Nested`](other.html)", ["Mod.Nested"]) == "[the `Mod.Nested`](other.html)"
  end

  # erlang functions

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
    assert Autolink.erlang_functions("`:unknown.foo/0`") == "`:unknown.foo/0`"
  end

  # typespec

  test "operators" do
    assert Autolink.typespec(quote(do: +number :: number), [], [], []) ==
           ~s[+number :: number]

    assert Autolink.typespec(quote(do: number + number :: number), [], [], []) ==
           ~s[number + number :: number]
  end

  test "strip parens in typespecs" do
    assert Autolink.typespec(quote(do: foo({}, bar())), [], []) == ~s[foo({}, bar)]
  end

  test "autolink locals in typespecs" do
    assert Autolink.typespec(quote(do: foo(1)), [foo: 1], []) ==
           ~s[<a href="#t:foo/1">foo(1)</a>]

    assert Autolink.typespec(quote(do: bar(foo(1))), [foo: 1], []) ==
           ~s[bar(<a href="#t:foo/1">foo(1)</a>)]

    assert Autolink.typespec(quote(do: (bar(foo(1)) when bat: foo(1))), [foo: 1], []) ==
           ~s[bar(<a href="#t:foo/1">foo(1)</a>) when bat: <a href=\"#t:foo/1\">foo(1)</a>]

    assert Autolink.typespec(quote(do: bar(foo(1))), [], []) ==
           ~s[bar(foo(1))]
  end

  test "autolink same type and function name" do
    assert Autolink.typespec(quote(do: foo() :: foo()), [foo: 0], [], []) ==
           ~s[foo() :: <a href="#t:foo/0">foo</a>]

    assert Autolink.typespec(quote(do: foo(1) :: foo(1)), [foo: 1], [], []) ==
           ~s[foo(1) :: <a href="#t:foo/1">foo(1)</a>]

    assert Autolink.typespec(quote(do: (foo(1) :: foo(1) when bat: foo(1))), [foo: 1], [], []) ==
           ~s[foo(1) :: <a href=\"#t:foo/1\">foo(1)</a> when bat: <a href=\"#t:foo/1\">foo(1)</a>]

    assert Autolink.typespec(quote(do: bar(foo(1)) :: foo(1)), [foo: 1], [], []) ==
           ~s[bar(<a href=\"#t:foo/1\">foo(1)</a>) :: <a href=\"#t:foo/1\">foo(1)</a>]

    assert Autolink.typespec(quote(do: (bar(foo(1)) :: foo(1) when bat: foo(1))), [foo: 1], [], []) ==
           ~s[bar(<a href=\"#t:foo/1\">foo(1)</a>) :: <a href=\"#t:foo/1\">foo(1)</a> when bat: <a href=\"#t:foo/1\">foo(1)</a>]

    assert Autolink.typespec(quote(do: bar(foo :: foo(1)) :: foo(1)), [foo: 1], [], []) ==
           ~s[bar(foo :: <a href=\"#t:foo/1\">foo(1)</a>) :: <a href=\"#t:foo/1\">foo(1)</a>]
  end

  test "add new lines on |" do
    assert Autolink.typespec(quote(do: (foo(1) :: bar | baz)), [], []) ==
           ~s[foo(1) :: bar | baz]

    assert Autolink.typespec(quote(do: (foo(1) :: bar | baz when bat: foo)), [], []) ==
           ~s[foo(1) :: bar | baz when bat: foo]

    assert Autolink.typespec(quote(do: (really_long_name_that_will_trigger_multiple_line_breaks(1) :: bar | baz)), [], []) ==
           ~s[really_long_name_that_will_trigger_multiple_line_breaks(1) ::\n  bar |\n  baz]

    assert Autolink.typespec(quote(do: (really_long_name_that_will_trigger_multiple_line_breaks(1) :: bar | baz when bat: foo)), [], []) ==
           ~s[really_long_name_that_will_trigger_multiple_line_breaks(1) ::\n  bar |\n  baz when bat: foo]
  end

  test "autolink Elixir types in typespecs" do
    assert Autolink.typespec(quote(do: String.t), [], []) ==
           ~s[<a href="https://hexdocs.pm/elixir/String.html#t:t/0">String.t</a>]

    assert Autolink.typespec(quote(do: Unknown.bar()), [], []) ==
           ~s[Unknown.bar]
  end

  test "autolink shared aliases in typespecs" do
    assert Autolink.typespec(quote(do: Foo.t), [], [Foo]) ==
           ~s[<a href="Foo.html#t:t/0">Foo.t</a>]
  end
end
