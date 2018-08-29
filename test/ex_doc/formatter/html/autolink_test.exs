defmodule ExDoc.Formatter.HTML.AutolinkTest do
  use ExUnit.Case, async: true
  alias ExDoc.Formatter.HTML.Autolink

  @elixir_docs "https://hexdocs.pm/"
  @erlang_docs "http://www.erlang.org/doc/man/"

  describe "local" do
    test "autolinks fun/arity in docs" do
      assert Autolink.locals("`example/2`", ["example/2"]) == "[`example/2`](#example/2)"

      assert Autolink.locals("`__ENV__/0`", ["__ENV__/0"]) == "[`__ENV__/0`](#__ENV__/0)"

      assert Autolink.locals("`example/2` then `example/2`", ["example/2"]) ==
               "[`example/2`](#example/2) then [`example/2`](#example/2)"

      assert Autolink.locals("`  spaces/0  `", ["spaces/0"]) == "[`spaces/0`](#spaces/0)"

      assert Autolink.locals("`example/1` and `example/2`", ["example/1", "example/2"]) ==
               "[`example/1`](#example/1) and [`example/2`](#example/2)"

      assert Autolink.locals(
               "`funny_name\?/1` and `funny_name!/2`",
               ["funny_name\?/1", "funny_name!/2"]
             ) == "[`funny_name\?/1`](#funny_name\?/1) and [`funny_name!/2`](#funny_name!/2)"

      assert Autolink.locals("`//2`", ["//2"]) == "[`//2`](#//2)"
    end

    test "autolinks to local callbacks" do
      # `split_function` must also return locally defined callbacks
      # format should be: "c:<fun>/<arity>"
      assert Autolink.locals("`c:fun/2`", ["c:fun/2"]) == "[`fun/2`](#c:fun/2)"
    end

    test "autolinks to local types" do
      assert Autolink.locals("`t:my_type/0`", ["t:my_type/0"]) == "[`my_type/0`](#t:my_type/0)"
      assert Autolink.locals("`t:my_type/1`", ["t:my_type/1"]) == "[`my_type/1`](#t:my_type/1)"
      # links to types without arity don't work
      assert Autolink.locals("`t:my_type`", ["t:my_type/0"]) == "`t:my_type`"
    end

    test "autolinks to basic and built-in types" do
      assert Autolink.locals("`t:atom/0`", []) ==
               "[`atom/0`](#{@elixir_docs}elixir/typespecs.html#basic-types)"

      assert Autolink.locals("`t:term/0`", []) ==
               "[`term/0`](#{@elixir_docs}elixir/typespecs.html#built-in-types)"
    end

    test "does not autolink undefined functions" do
      assert Autolink.locals("`example/1`", ["example/2"]) == "`example/1`"
      assert Autolink.locals("`example/1`", []) == "`example/1`"
    end

    test "does not autolink pre-linked functions" do
      assert Autolink.locals("[`example/1`]()", ["example/1"]) == "[`example/1`]()"
      assert Autolink.locals("[the `example/1`]()", ["example/1"]) == "[the `example/1`]()"
    end

    test "autolinks special forms" do
      assert Autolink.locals("`{}/1`", ["{}/1"]) === "[`{}/1`](#%7B%7D/1)"
      assert Autolink.locals("`<<>>/1`", ["<<>>/1"]) === "[`<<>>/1`](#%3C%3C%3E%3E/1)"

      assert Autolink.locals("`<<>>/1`", []) ===
               "[`<<>>/1`](#{@elixir_docs}elixir/Kernel.SpecialForms.html#%3C%3C%3E%3E/1)"

      assert Autolink.locals("`<<>>/1`", [], [Kernel]) ===
               "[`<<>>/1`](Kernel.SpecialForms.html#%3C%3C%3E%3E/1)"
    end

    test "autolinks Kernel functions" do
      assert Autolink.locals("`abs/1`", ["abs/1"]) === "[`abs/1`](#abs/1)"

      assert Autolink.locals("`abs/1`", []) ===
               "[`abs/1`](#{@elixir_docs}elixir/Kernel.html#abs/1)"

      assert Autolink.locals("`!/1`", ["!/1"]) === "[`!/1`](#!/1)"
      assert Autolink.locals("`!/1`", []) === "[`!/1`](#{@elixir_docs}elixir/Kernel.html#!/1)"
      assert Autolink.locals("`!/1`", [], [Kernel]) === "[`!/1`](Kernel.html#!/1)"
    end
  end

  describe "elixir functions" do
    test "autolinks Module.fun/arity in docs" do
      assert Autolink.elixir_functions("`Mod.example/2`", ["Mod.example/2"]) ==
               "[`Mod.example/2`](Mod.html#example/2)"

      assert Autolink.elixir_functions("`Mod.__ENV__/2`", ["Mod.__ENV__/2"]) ==
               "[`Mod.__ENV__/2`](Mod.html#__ENV__/2)"

      assert Autolink.elixir_functions("`MyModule.example/2`", ["MyModule.example/2"]) ==
               "[`MyModule.example/2`](MyModule.html#example/2)"

      assert Autolink.elixir_functions(
               "`MyModule.Nested.example/2`",
               ["MyModule.Nested.example/2"]
             ) == "[`MyModule.Nested.example/2`](MyModule.Nested.html#example/2)"

      assert Autolink.elixir_functions("`Mod.example/2` then `Mod.example/2`", ["Mod.example/2"]) ==
               "[`Mod.example/2`](Mod.html#example/2) then [`Mod.example/2`](Mod.html#example/2)"

      assert Autolink.elixir_functions("`  MyModule.spaces/0  `", ["MyModule.spaces/0"]) ==
               "[`MyModule.spaces/0`](MyModule.html#spaces/0)"

      assert Autolink.elixir_functions(
               "`ModA.example/1` and `ModB.example/2`",
               ["ModA.example/1", "ModB.example/2"]
             ) ==
               "[`ModA.example/1`](ModA.html#example/1) and [`ModB.example/2`](ModB.html#example/2)"

      assert Autolink.elixir_functions(
               "`Mod.funny_name\?/1` and `Mod.funny_name!/2`",
               ["Mod.funny_name\?/1", "Mod.funny_name!/2"]
             ) ==
               "[`Mod.funny_name\?/1`](Mod.html#funny_name\?/1) and [`Mod.funny_name!/2`](Mod.html#funny_name!/2)"
    end

    test "autolinks functions Module.fun/arity in elixir" do
      assert Autolink.elixir_functions("`String.upcase/1`", ["Mod.example/2"]) ==
               "[`String.upcase/1`](#{@elixir_docs}elixir/String.html#upcase/1)"

      assert Autolink.elixir_functions("`Mix.env/0`", ["Mod.example/2"]) ==
               "[`Mix.env/0`](#{@elixir_docs}mix/Mix.html#env/0)"
    end

    test "autolinks to the longest libdir" do
      lib_dir = :code.where_is_file('Elixir.CompiledWithDocs.beam')

      lib_dirs = [
        {Path.dirname(Path.dirname(lib_dir)), "http://short/"},
        {Path.dirname(lib_dir), "http://long/"}
      ]

      assert Autolink.elixir_functions("`CompiledWithDocs.example/2`", [], ".html", lib_dirs) ==
               "[`CompiledWithDocs.example/2`](http://long/CompiledWithDocs.html#example/2)"
    end

    test "autolinks special forms" do
      assert Autolink.elixir_functions("`Mod.++/2`", ["Mod.++/2"]) ===
               "[`Mod.++/2`](Mod.html#++/2)"

      assert Autolink.elixir_functions("`Mod.!/1`", ["Mod.!/1"]) === "[`Mod.!/1`](Mod.html#!/1)"

      assert Autolink.elixir_functions("`Mod.../2`", ["Mod.../2"]) ===
               "[`Mod.../2`](Mod.html#../2)"

      assert Autolink.elixir_functions("`Mod.--/2`", ["Mod.--/2"]) ===
               "[`Mod.--/2`](Mod.html#--/2)"

      assert Autolink.elixir_functions("`Mod.%/2`", ["Mod.%/2"]) === "[`Mod.%/2`](Mod.html#%25/2)"

      assert Autolink.elixir_functions("`Mod.<<>>/1`", ["Mod.<<>>/1"]) ===
               "[`Mod.<<>>/1`](Mod.html#%3C%3C%3E%3E/1)"
    end

    test "autolinks callbacks" do
      assert Autolink.elixir_functions("`c:Mod.++/2`", ["c:Mod.++/2"]) ===
               "[`Mod.++/2`](Mod.html#c:++/2)"
    end

    test "autolinks types" do
      # use the same approach for elixir_functions as for locals
      assert Autolink.elixir_functions(
               "`t:MyModule.my_type/0`",
               ["t:MyModule.my_type/0"]
             ) == "[`MyModule.my_type/0`](MyModule.html#t:my_type/0)"

      assert Autolink.elixir_functions(
               "`t:MyModule.my_type`",
               ["t:MyModule.my_type/0"]
             ) == "`t:MyModule.my_type`"
    end

    test "does not autolink undefined Mod.functions" do
      assert Autolink.elixir_functions("`Mod.example/1`", ["Mod.example/2"]) == "`Mod.example/1`"
      assert Autolink.elixir_functions("`Mod.example/1`", []) == "`Mod.example/1`"
    end

    test "does not autolink pre-linked Mod.functions" do
      assert Autolink.elixir_functions("[`Mod.example/1`]()", ["Mod.example/1"]) ==
               "[`Mod.example/1`]()"

      assert Autolink.elixir_functions("[the `Mod.example/1`]()", ["Mod.example/1"]) ==
               "[the `Mod.example/1`]()"

      assert Autolink.elixir_functions("[`Mod.example/1`](foo)", ["Mod.example/1"]) ==
               "[`Mod.example/1`](foo)"

      assert Autolink.elixir_functions("[the `Mod.example/1`](foo)", ["Mod.example/1"]) ==
               "[the `Mod.example/1`](foo)"
    end

    test "supports normal links" do
      assert Autolink.elixir_functions("`Mod.example/1`", ["Mod.example/1"]) ==
               "[`Mod.example/1`](Mod.html#example/1)"

      assert Autolink.elixir_functions("(`Mod.example/1`)", ["Mod.example/1"]) ==
               "([`Mod.example/1`](Mod.html#example/1))"

      # It ignores links preceded by "]("
      assert Autolink.elixir_functions("](`Mod.example/1`)", ["Mod.example/1"]) ==
               "](`Mod.example/1`)"
    end

    test "supports custom links" do
      assert Autolink.elixir_functions("[`example`](`Mod.example/1`)", ["Mod.example/1"]) ==
               "[`example`](Mod.html#example/1)"

      assert Autolink.elixir_functions("[the `example`](`Mod.example/1`)", ["Mod.example/1"]) ==
               "[the `example`](Mod.html#example/1)"

      assert Autolink.elixir_functions("[the `Mod.example/1`](`Mod.example/1`)", ["Mod.example/1"]) ==
               "[the `Mod.example/1`](Mod.html#example/1)"

      assert Autolink.elixir_functions("[`callback(a)`](`c:Foo.foo/1`)", ["c:Foo.foo/1"]) ==
               "[`callback(a)`](Foo.html#c:foo/1)"

      assert Autolink.elixir_functions("[the `upcase`](`String.upcase/1`)", []) ==
               "[the `upcase`](#{@elixir_docs}elixir/String.html#upcase/1)"

      assert Autolink.elixir_functions("[`f`](`Foo.foo/1`), [`f`](`Foo.foo/1`)", ["Foo.foo/1"]) ==
               "[`f`](Foo.html#foo/1), [`f`](Foo.html#foo/1)"

      assert Autolink.elixir_functions("[`foo`](`Foo.//2`)", ["Foo.//2"]) ==
               "[`foo`](Foo.html#//2)"

      assert Autolink.elixir_functions("[`for`](`Kernel.SpecialForms.for/1`)", []) ==
               "[`for`](#{@elixir_docs}elixir/Kernel.SpecialForms.html#for/1)"

      assert Autolink.elixir_functions("[`for`](`for/1`)", []) ==
               "[`for`](#{@elixir_docs}elixir/Kernel.SpecialForms.html#for/1)"

      assert Autolink.elixir_functions("[`is_boolean`](`Kernel.is_boolean/1`)", []) ==
               "[`is_boolean`](#{@elixir_docs}elixir/Kernel.html#is_boolean/1)"

      assert Autolink.elixir_functions("[`is_boolean`](`is_boolean/1`)", []) ==
               "[`is_boolean`](#{@elixir_docs}elixir/Kernel.html#is_boolean/1)"

      assert Autolink.elixir_functions("[term()](`t:term/0`)", []) ==
               "[term()](#{@elixir_docs}elixir/typespecs.html#built-in-types)"

      assert Autolink.elixir_functions("[term\(\)](`t:term/0`)", []) ==
               "[term\(\)](#{@elixir_docs}elixir/typespecs.html#built-in-types)"

      assert Autolink.elixir_functions("[`term()`](`t:term/0`)", []) ==
               "[`term()`](#{@elixir_docs}elixir/typespecs.html#built-in-types)"

      assert Autolink.elixir_functions("[`term()`](`t:term/0`)", []) ==
               "[`term()`](#{@elixir_docs}elixir/typespecs.html#built-in-types)"

      assert Autolink.elixir_functions("[version](`t:Version.version/0`)", ["t:Version.version/0"]) ==
               "[version](Version.html#t:version/0)"

      assert Autolink.link_everything("[version](`t:Version.version/0`)",
               docs_refs: ["t:Version.version/0"]
             ) == "[version](Version.html#t:version/0)"

      # assert Autolink.link_everything("[version](`t:version/0`)", [locals: ["t:Version.version/0"]]) ==
      assert Autolink.link_everything("[version](`t:version/0`)", locals: ["t:version/0"]) ==
               "[version](#t:version/0)"
    end
  end

  describe "elixir modules" do
    test "autolinks modules in docs" do
      assert Autolink.elixir_modules("`MyModule`", ["MyModule"], "MyModule") ==
               "[`MyModule`](MyModule.html#content)"

      assert Autolink.elixir_modules("`MyModule.Nested`", ["MyModule.Nested"], "MyModule.Nested") ==
               "[`MyModule.Nested`](MyModule.Nested.html#content)"

      assert Autolink.elixir_modules(
               "`MyModule.Nested.Deep`",
               ["MyModule.Nested.Deep"],
               "MyModule.Nested.Deep"
             ) == "[`MyModule.Nested.Deep`](MyModule.Nested.Deep.html#content)"

      assert Autolink.elixir_modules(
               "```\nThis is a test.\n```\n\nSee `MyModule`.",
               ["MyModule"],
               "MyModule"
             ) == "```\nThis is a test.\n```\n\nSee [`MyModule`](MyModule.html#content)."
    end

    test "autolinks modules in elixir" do
      assert Autolink.elixir_modules("`String`", ["MyModule"], "MyModule") ==
               "[`String`](#{@elixir_docs}elixir/String.html)"

      assert Autolink.elixir_modules("`Mix`", ["MyModule"], "MyModule") ==
               "[`Mix`](#{@elixir_docs}mix/Mix.html)"
    end

    test "autolinks dependencies modules" do
      lib_dirs = [{Application.app_dir(:earmark), "#{@elixir_docs}earmark/"}]

      assert Autolink.elixir_modules("`Earmark`", ["MyModule"], "MyModule", ".html", lib_dirs) ==
               "[`Earmark`](#{@elixir_docs}earmark/Earmark.html)"
    end

    test "does not autolink undefined modules" do
      assert Autolink.elixir_modules("`MyModule`", []) == "`MyModule`"
      assert Autolink.elixir_modules("`MyModule`", ["DiffModule"]) == "`MyModule`"

      assert Autolink.elixir_modules("`MyModule.Nested`", ["MyModule.DiffNested"]) ==
               "`MyModule.Nested`"
    end

    test "does not autolink pre-linked modules" do
      assert Autolink.elixir_modules("[`Mod`](other.html)", ["Mod"]) == "[`Mod`](other.html)"

      assert Autolink.elixir_modules("[the `Mod`](other.html)", ["Mod"]) ==
               "[the `Mod`](other.html)"

      assert Autolink.elixir_modules("[the `Mod.Nested`](other.html)", ["Mod.Nested"]) ==
               "[the `Mod.Nested`](other.html)"

      assert Autolink.elixir_modules("[in the `Kernel` module](Kernel.html#guards)", ["Kernel"]) ==
               "[in the `Kernel` module](Kernel.html#guards)"

      assert Autolink.elixir_modules("[in the `Kernel` module](Kernel.html#guards)", []) ==
               "[in the `Kernel` module](Kernel.html#guards)"

      assert Autolink.link_everything("[in the `Kernel` module](Kernel.html#guards)", []) ==
               "[in the `Kernel` module](Kernel.html#guards)"
    end
  end

  describe "Erlang modules" do
    test "autolinks to Erlang modules" do
      assert Autolink.erlang_modules("`:erlang`") == "[`:erlang`](#{@erlang_docs}erlang.html)"

      assert Autolink.erlang_modules("`:erl_prim_loader`") ==
               "[`:erl_prim_loader`](#{@erlang_docs}erl_prim_loader.html)"
    end

    test "autolinks to Erlang modules with custom links" do
      assert Autolink.erlang_modules("[`example`](`:lists`)") ==
               "[`example`](#{@erlang_docs}lists.html)"

      assert Autolink.erlang_modules("[example](`:lists`)") ==
               "[example](#{@erlang_docs}lists.html)"
    end

    test "does not autolink pre-linked docs" do
      assert Autolink.erlang_modules("[`:erlang`](other.html)") == "[`:erlang`](other.html)"

      assert Autolink.erlang_modules("[the `:erlang` module](other.html)") ==
               "[the `:erlang` module](other.html)"

      assert Autolink.erlang_modules("`:erlang`") == "[`:erlang`](#{@erlang_docs}erlang.html)"
    end

    test "does not autolink functions that aren't part of the Erlang distribution" do
      assert Autolink.erlang_modules("`:unknown.foo/0`") == "`:unknown.foo/0`"
    end
  end

  describe "erlang functions" do
    test "autolinks to erlang functions" do
      assert Autolink.erlang_functions("`:erlang.apply/2`") ==
               "[`:erlang.apply/2`](#{@erlang_docs}erlang.html#apply-2)"

      assert Autolink.erlang_functions("`:erlang.adler32/2`") ==
               "[`:erlang.adler32/2`](#{@erlang_docs}erlang.html#adler32-2)"

      assert Autolink.erlang_functions("`:erlang.apply/2` `:erlang.apply/3`") ==
               "[`:erlang.apply/2`](#{@erlang_docs}erlang.html#apply-2) [`:erlang.apply/3`](#{
                 @erlang_docs
               }erlang.html#apply-3)"

      assert Autolink.erlang_functions("`:erl_prim_loader.get_file/1`") ==
               "[`:erl_prim_loader.get_file/1`](#{@erlang_docs}erl_prim_loader.html#get_file-1)"

      assert Autolink.erlang_functions("`:zlib.deflateInit/2`") ==
               "[`:zlib.deflateInit/2`](#{@erlang_docs}zlib.html#deflateInit-2)"
    end

    test "autolinks to Erlang functions with custom links" do
      assert Autolink.erlang_functions("[`example`](`:lists.reverse/1`)") ==
               "[`example`](#{@erlang_docs}lists.html#reverse-1)"

      assert Autolink.erlang_functions("[example](`:lists.reverse/1`)") ==
               "[example](#{@erlang_docs}lists.html#reverse-1)"
    end

    test "does not autolink pre-linked docs" do
      assert Autolink.erlang_functions("[`:erlang.apply/2`](other.html)") ==
               "[`:erlang.apply/2`](other.html)"

      assert Autolink.erlang_functions("[the `:erlang.apply/2`](other.html)") ==
               "[the `:erlang.apply/2`](other.html)"

      assert Autolink.erlang_functions("[the `:erlang.apply/2` function](`Kernel.apply/2`)") ==
               "[the `:erlang.apply/2` function](`Kernel.apply/2`)"

      assert Autolink.erlang_functions("[the :erlang.apply/2 function](`Kernel.apply/2`)") ==
               "[the :erlang.apply/2 function](`Kernel.apply/2`)"

      assert Autolink.erlang_functions("[the `:erlang.apply/2` function](other.html)") ==
               "[the `:erlang.apply/2` function](other.html)"

      assert Autolink.erlang_functions("`:erlang`") == "`:erlang`"
    end

    test "does not autolink for functions that aren't part of the erlang distribution" do
      assert Autolink.erlang_functions("`:unknown.foo/0`") == "`:unknown.foo/0`"
    end
  end

  describe "typespecs" do
    test "formats operators" do
      assert Autolink.typespec(quote(do: +foo() :: foo()), [], [], []) == ~s[+foo() :: foo()]

      assert Autolink.typespec(quote(do: foo() + foo() :: foo()), [], [], []) ==
               ~s[foo() + foo() :: foo()]
    end

    test "strips parens" do
      assert Autolink.typespec(quote(do: foo({}, bar())), [], []) == ~s[foo({}, bar())]
    end

    test "autolinks locals" do
      assert Autolink.typespec(quote(do: foo(1)), [foo: 1], []) ==
               ~s[<a href="#t:foo/1">foo</a>(1)]

      assert Autolink.typespec(quote(do: bar(foo(1))), [foo: 1], []) ==
               ~s[bar(<a href="#t:foo/1">foo</a>(1))]

      assert Autolink.typespec(quote(do: (bar(foo(1)) when bat: foo(1))), [foo: 1], []) ==
               ~s[bar(<a href="#t:foo/1">foo</a>(1)) when bat: <a href=\"#t:foo/1\">foo</a>(1)]

      assert Autolink.typespec(quote(do: bar(foo(1))), [], []) == ~s[bar(foo(1))]
    end

    test "autolinks same type and function name" do
      assert Autolink.typespec(quote(do: foo() :: foo()), [foo: 0], [], []) ==
               ~s[foo() :: <a href="#t:foo/0">foo</a>()]

      assert Autolink.typespec(quote(do: foo(1) :: foo(1)), [foo: 1], [], []) ==
               ~s[foo(1) :: <a href="#t:foo/1">foo</a>(1)]

      assert Autolink.typespec(quote(do: (foo(1) :: foo(1) when bat: foo(1))), [foo: 1], [], []) ==
               ~s[foo(1) :: <a href=\"#t:foo/1\">foo</a>(1) when bat: <a href=\"#t:foo/1\">foo</a>(1)]

      assert Autolink.typespec(quote(do: bar(foo(1)) :: foo(1)), [foo: 1], [], []) ==
               ~s[bar(<a href=\"#t:foo/1\">foo</a>(1)) :: <a href=\"#t:foo/1\">foo</a>(1)]

      assert Autolink.typespec(quote(do: (bar(f(1)) :: f(1) when bat: f(1))), [f: 1], [], []) ==
               ~s[bar(<a href=\"#t:f/1\">f</a>(1)) :: <a href=\"#t:f/1\">f</a>(1) when bat: <a href=\"#t:f/1\">f</a>(1)]

      assert Autolink.typespec(quote(do: bar(foo :: foo(1)) :: foo(1)), [foo: 1], [], []) ==
               ~s[bar(foo :: <a href=\"#t:foo/1\">foo</a>(1)) :: <a href=\"#t:foo/1\">foo</a>(1)]
    end

    test "autolinks Elixir types" do
      assert Autolink.typespec(quote(do: String.t()), [], []) ==
               ~s[<a href="#{@elixir_docs}elixir/String.html#t:t/0">String.t</a>()]

      assert Autolink.typespec(quote(do: Unknown.bar()), [], []) == ~s[Unknown.bar()]
    end

    test "autolinks Elixir basic types" do
      assert Autolink.typespec(quote(do: atom()), [], []) ==
               ~s[<a href=\"#{@elixir_docs}elixir/typespecs.html#basic-types\">atom</a>()]
    end

    test "autolinks Elixir built-in types" do
      assert Autolink.typespec(quote(do: term()), [], []) ==
               ~s[<a href=\"#{@elixir_docs}elixir/typespecs.html#built-in-types\">term</a>()]

      assert Autolink.typespec(quote(do: term()), [], [Kernel]) ==
               ~s[<a href=\"typespecs.html#built-in-types\">term</a>()]
    end

    test "autolinks Erlang types" do
      assert Autolink.typespec(quote(do: :sets.set()), [], []) ==
               ~s[<a href=\"#{@erlang_docs}sets.html#type-set\">:sets.set</a>()]

      assert Autolink.typespec(quote(do: :sets.set(foo())), [], []) ==
               ~s[<a href=\"#{@erlang_docs}sets.html#type-set\">:sets.set</a>(foo())]

      assert Autolink.typespec(quote(do: :sets.set(foo())), [foo: 0], []) ==
               ~s[<a href=\"#{@erlang_docs}sets.html#type-set\">:sets.set</a>(<a href=\"#t:foo/0\">foo</a>())]
    end

    test "autolinks shared aliases" do
      assert Autolink.typespec(quote(do: Foo.t()), [], [Foo]) ==
               ~s[<a href="Foo.html#t:t/0">Foo.t</a>()]
    end

    test "autolinks inside parameterized types" do
      assert Autolink.typespec(quote(do: t(foo())), [t: 1, foo: 0], []) ==
               ~s[<a href="#t:t/1">t</a>(<a href="#t:foo/0">foo</a>())]

      assert Autolink.typespec(quote(do: Parameterized.t(foo())), [foo: 0], [Parameterized]) ==
               ~s[<a href="Parameterized.html#t:t/1">Parameterized.t</a>(<a href="#t:foo/0">foo</a>())]

      assert Autolink.typespec(quote(do: parameterized_t(Foo.t())), [parameterized_t: 1], [Foo]) ==
               ~s[<a href="#t:parameterized_t/1">parameterized_t</a>(<a href="Foo.html#t:t/0">Foo.t</a>())]

      assert Autolink.typespec(quote(do: Parameterized.t(Foo.t())), [], [Parameterized, Foo]) ==
               ~s[<a href="Parameterized.html#t:t/1">Parameterized.t</a>(<a href="Foo.html#t:t/0">Foo.t</a>())]

      assert Autolink.typespec(quote(do: t(foo() | bar())), [t: 1, foo: 0, bar: 0], []) ==
               ~s[<a href="#t:t/1">t</a>(<a href="#t:foo/0">foo</a>() | <a href="#t:bar/0">bar</a>())]

      assert Autolink.typespec(quote(do: t(t(foo()))), [t: 1, foo: 0], []) ==
               ~s[<a href="#t:t/1">t</a>(<a href="#t:t/1">t</a>(<a href="#t:foo/0">foo</a>()))]

      assert Autolink.typespec(quote(do: parameterized_t(foo())), [foo: 0], []) ==
               ~s[parameterized_t(<a href="#t:foo/0">foo</a>())]

      assert Autolink.typespec(quote(do: parameterized_t(atom())), [], []) ==
               ~s[parameterized_t(<a href=\"#{@elixir_docs}elixir/typespecs.html#basic-types\">atom</a>())]

      assert Autolink.typespec(quote(do: parameterized_t(atom()) :: list(function())), [], []) ==
               ~s[parameterized_t(<a href=\"#{@elixir_docs}elixir/typespecs.html#basic-types\">atom</a>()) :: ] <>
                 ~s[<a href=\"#{@elixir_docs}elixir/typespecs.html#basic-types\">list</a>(] <>
                 ~s[<a href=\"#{@elixir_docs}elixir/typespecs.html#built-in-types\">function</a>())]
    end

    test "placeholders" do
      assert_typespec_placeholders(
        "t()",
        "_p1_()",
        t: 0
      )

      assert_typespec_placeholders(
        "foobar()",
        "_ppp1_()",
        foobar: 0
      )

      assert_typespec_placeholders(
        "Mod.foobar()",
        "_ppppppp1_()",
        [],
        [Mod]
      )

      assert_typespec_placeholders(
        "foobar(barbaz())",
        "_ppp1_(_ppp2_())",
        foobar: 1,
        barbaz: 0
      )

      assert_typespec_placeholders(
        "Mod.foobar(Mod.barbaz())",
        "_ppppppp1_(_ppppppp2_())",
        [],
        [Mod]
      )

      assert_typespec_placeholders(
        "foobar(foobar(barbaz()))",
        "_ppp1_(_ppp1_(_ppp2_()))",
        foobar: 1,
        barbaz: 0
      )
    end
  end

  describe "corner-cases" do
    test "accepts functions around () and []" do
      assert Autolink.locals("`===/2`", [], [Kernel]) === "[`===/2`](Kernel.html#===/2)"
      assert Autolink.locals("(`===/2`)", [], [Kernel]) === "([`===/2`](Kernel.html#===/2))"
      assert Autolink.locals("[`===/2`]", [], [Kernel]) === "[[`===/2`](Kernel.html#===/2)]"

      output = Autolink.link_everything("`===/2`", [])
      assert output === "[`===/2`](#{@elixir_docs}elixir/Kernel.html#===/2)"
      assert Autolink.link_everything("(`===/2`)", []) === "(" <> output <> ")"
      assert Autolink.link_everything("[`===/2`]", []) === "[" <> output <> "]"
    end
  end

  defp assert_typespec_placeholders(original, expected, typespecs, aliases \\ []) do
    ast = Code.string_to_quoted!(original)
    {actual, _} = Autolink.format_and_extract_typespec_placeholders(ast, typespecs, aliases, [])
    assert actual == expected, "Original: #{original}\nExpected: #{expected}\nActual:   #{actual}"
  end

  describe "backtick preprocessing" do
    test "replace backticks" do
      assert Autolink.preprocess("[`===/2`](foo)") ===
               "[#{Autolink.backtick_token()}===/2#{Autolink.backtick_token()}](foo)"
    end

    test "do not touch backticks" do
      assert Autolink.preprocess("`===/2`") === "`===/2`"
      assert Autolink.preprocess("(`===/2`)") === "(`===/2`)"
      assert Autolink.preprocess("(foo)[`Module`]") === "(foo)[`Module`]"

      # this tests a bug in the regex that was being too greedy and stretching for several links
      string = """
      A [version](`t:version/0`) is a [string](`t:String.t/0`) in a specific
      format or a [version](`t:Version.t/0`) struct
      generated after parsing a version string with `Version.parse/1`.
      """

      assert Autolink.preprocess(string) === string
    end

    test "replace backtick tokens" do
      assert Autolink.postprocess(
               "[#{Autolink.backtick_token()}===/2#{Autolink.backtick_token()}](foo)"
             ) === "[`===/2`](foo)"

      string = """
      [A `version` is](`t:version/0`) a [beautiful `string` in a](`t:String.t/0`) specific
      format or a [`version`](`t:Version.t/0`) struct
      generated after parsing a version string with `Version.parse/1`.
      """

      refute Autolink.preprocess(string) === string
      assert string |> Autolink.preprocess() |> Autolink.postprocess() === string
    end
  end
end
