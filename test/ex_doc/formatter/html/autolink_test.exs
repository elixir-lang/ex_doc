defmodule ExDoc.Formatter.HTML.AutolinkTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureIO
  alias ExDoc.Formatter.HTML.Autolink

  @elixir_docs "https://hexdocs.pm/"
  @erlang_docs "http://www.erlang.org/doc/man/"

  describe "local" do
    test "autolinks fun/arity in docs" do
      assert project_doc("`example/2`", %{locals: ["example/2"]}) ==
               "[`example/2`](#example/2 'example/2')"

      assert project_doc("`__ENV__/0`", %{locals: ["__ENV__/0"]}) ==
               "[`__ENV__/0`](#__ENV__/0 '__ENV__/0')"

      assert project_doc("`example/2` then `example/2`", %{locals: ["example/2"]}) ==
               "[`example/2`](#example/2 'example/2') then [`example/2`](#example/2 'example/2')"

      assert project_doc("`  spaces/0  `", %{locals: ["spaces/0"]}) ==
               "[`spaces/0`](#spaces/0 'spaces/0')"

      assert project_doc("`example/1` and `example/2`", %{locals: ["example/1", "example/2"]}) ==
               "[`example/1`](#example/1 'example/1') and [`example/2`](#example/2 'example/2')"

      assert project_doc(
               "`funny_name\?/1` and `funny_name!/2`",
               %{locals: ["funny_name\?/1", "funny_name!/2"]}
             ) ==
               "[`funny_name\?/1`](#funny_name\?/1 'funny_name\?/1') and [`funny_name!/2`](#funny_name!/2 'funny_name!/2')"

      assert project_doc("`//2`", %{locals: ["//2"]}) == "[`//2`](#//2 '//2')"
    end

    test "autolinks to local callbacks" do
      # `split_function` must also return locally defined callbacks
      # format should be: "c:<fun>/<arity>"
      assert project_doc("`c:fun/2`", %{locals: ["c:fun/2"]}) == "[`fun/2`](#c:fun/2 'c:fun/2')"
    end

    test "autolinks to local types" do
      assert project_doc("`t:my_type/0`", %{locals: ["t:my_type/0"]}) ==
               "[`my_type/0`](#t:my_type/0 't:my_type/0')"

      assert project_doc("`t:my_type/1`", %{locals: ["t:my_type/1"]}) ==
               "[`my_type/1`](#t:my_type/1 't:my_type/1')"

      # links to types without arity don't work
      assert project_doc("`t:my_type`", %{locals: ["t:my_type/0"]}) == "`t:my_type`"
    end

    test "autolinks to basic and built-in types" do
      assert project_doc("`t:atom/0`", %{}) ==
               "[`atom/0`](#{@elixir_docs}elixir/typespecs.html#basic-types 'Basic types — Typespecs')"

      assert project_doc("`t:term/0`", %{}) ==
               "[`term/0`](#{@elixir_docs}elixir/typespecs.html#built-in-types 'Built-in types — Typespecs')"
    end

    test "does not autolink undefined functions" do
      assert project_doc("`example/1`", %{locals: ["example/2"]}) == "`example/1`"
      assert project_doc("`example/1`", %{}) == "`example/1`"
    end

    test "does not autolink pre-linked functions" do
      assert project_doc("[`example/1`]()", %{locals: ["example/1"]}) == "[`example/1`]()"
      assert project_doc("[the `example/1`]()", %{locals: ["example/1"]}) == "[the `example/1`]()"
    end

    test "autolinks special forms" do
      assert project_doc("`{}/1`", %{locals: ["{}/1"]}) === "[`{}/1`](#%7B%7D/1 '{}/1')"

      assert project_doc("`<<>>/1`", %{locals: ["<<>>/1"]}) ===
               "[`<<>>/1`](#%3C%3C%3E%3E/1 '<<>>/1')"

      assert project_doc("`<<>>/1`", %{}) ===
               "[`<<>>/1`](#{@elixir_docs}elixir/Kernel.SpecialForms.html#%3C%3C%3E%3E/1 'Kernel.SpecialForms.<<>>/1')"

      assert project_doc("`<<>>/1`", %{aliases: [Kernel]}) ===
               "[`<<>>/1`](Kernel.SpecialForms.html#%3C%3C%3E%3E/1 'Kernel.SpecialForms.<<>>/1')"
    end

    test "autolinks Kernel functions" do
      assert project_doc("`abs/1`", %{locals: ["abs/1"]}) === "[`abs/1`](#abs/1 'abs/1')"

      assert project_doc("`abs/1`", %{}) ===
               "[`abs/1`](#{@elixir_docs}elixir/Kernel.html#abs/1 'Kernel.abs/1')"

      assert project_doc("`!/1`", %{locals: ["!/1"]}) === "[`!/1`](#!/1 '!/1')"

      assert project_doc("`!/1`", %{}) ===
               "[`!/1`](#{@elixir_docs}elixir/Kernel.html#!/1 'Kernel.!/1')"

      assert project_doc("`!/1`", %{aliases: [Kernel]}) ===
               "[`!/1`](Kernel.html#!/1 'Kernel.!/1')"
    end
  end

  describe "elixir functions" do
    test "autolinks Module.fun/arity in docs" do
      assert project_doc("`Mod.example/2`", %{docs_refs: ["Mod.example/2"]}) ==
               "[`Mod.example/2`](Mod.html#example/2 'Mod.example/2')"

      assert project_doc("`Mod.__ENV__/2`", %{docs_refs: ["Mod.__ENV__/2"]}) ==
               "[`Mod.__ENV__/2`](Mod.html#__ENV__/2 'Mod.__ENV__/2')"

      assert project_doc("`MyModule.example/2`", %{docs_refs: ["MyModule.example/2"]}) ==
               "[`MyModule.example/2`](MyModule.html#example/2 'MyModule.example/2')"

      assert project_doc(
               "`MyModule.Nested.example/2`",
               %{docs_refs: ["MyModule.Nested.example/2"]}
             ) ==
               "[`MyModule.Nested.example/2`](MyModule.Nested.html#example/2 'MyModule.Nested.example/2')"

      assert project_doc("`Mod.example/2` then `Mod.example/2`", %{docs_refs: ["Mod.example/2"]}) ==
               "[`Mod.example/2`](Mod.html#example/2 'Mod.example/2') then [`Mod.example/2`](Mod.html#example/2 'Mod.example/2')"

      assert project_doc("`  MyModule.spaces/0  `", %{docs_refs: ["MyModule.spaces/0"]}) ==
               "[`MyModule.spaces/0`](MyModule.html#spaces/0 'MyModule.spaces/0')"

      assert project_doc(
               "`ModA.example/1` and `ModB.example/2`",
               %{docs_refs: ["ModA.example/1", "ModB.example/2"]}
             ) ==
               "[`ModA.example/1`](ModA.html#example/1 'ModA.example/1') and [`ModB.example/2`](ModB.html#example/2 'ModB.example/2')"

      assert project_doc(
               "`Mod.funny_name\?/1` and `Mod.funny_name!/2`",
               %{docs_refs: ["Mod.funny_name\?/1", "Mod.funny_name!/2"]}
             ) ==
               "[`Mod.funny_name\?/1`](Mod.html#funny_name\?/1 'Mod.funny_name\?/1') and [`Mod.funny_name!/2`](Mod.html#funny_name!/2 'Mod.funny_name!/2')"
    end

    test "warns when module exists but the function does not" do
      compiled = %{
        docs_refs: ["Mod.example/1"],
        modules_refs: ["Mod"],
        skip_undefined_reference_warnings_on: ["Bar", "deprecations"]
      }

      assert capture_io(:stderr, fn ->
               assert Autolink.project_doc("`Mod.example/2`", "Mod.foo/0", compiled) ==
                        "`Mod.example/2`"
             end) =~ ~r"references Mod.example/2 .* \(parsing Mod.foo/0 docs\)"

      assert capture_io(:stderr, fn ->
               assert Autolink.project_doc("`Mod.example/2`", "extras", compiled) ==
                        "`Mod.example/2`"
             end) =~ ~r"references Mod.example/2 .* \(parsing extras docs\)"

      # skip warning when module page is blacklisted
      overwritten = Map.put(compiled, :module_id, "Bar")
      assert assert project_doc("`Mod.example/2`", "Mod.bar/0", overwritten) == "`Mod.example/2`"

      # skip warning when extras page is blacklisted
      assert assert project_doc("`Mod.example/2`", "deprecations", compiled) == "`Mod.example/2`"
    end

    test "autolinks functions Module.fun/arity in elixir" do
      assert project_doc("`String.upcase/1`", %{docs_refs: ["Mod.example/2"]}) ==
               "[`String.upcase/1`](#{@elixir_docs}elixir/String.html#upcase/1 'String.upcase/1')"

      assert project_doc("`Mix.env/0`", %{docs_refs: ["Mod.example/2"]}) ==
               "[`Mix.env/0`](#{@elixir_docs}mix/Mix.html#env/0 'Mix.env/0')"
    end

    test "autolinks to the longest libdir" do
      lib_dir = :code.where_is_file('Elixir.CompiledWithDocs.beam')

      lib_dirs = [
        {Path.dirname(Path.dirname(lib_dir)), "http://short/"},
        {Path.dirname(lib_dir), "http://long/"}
      ]

      assert project_doc("`CompiledWithDocs.example/2`", %{
               docs_refs: [],
               extension: ".html",
               lib_dirs: lib_dirs
             }) ==
               "[`CompiledWithDocs.example/2`](http://long/CompiledWithDocs.html#example/2 'CompiledWithDocs.example/2')"
    end

    test "autolinks special forms" do
      assert project_doc("`Mod.++/2`", %{docs_refs: ["Mod.++/2"]}) ===
               "[`Mod.++/2`](Mod.html#++/2 'Mod.++/2')"

      assert project_doc("`Mod.!/1`", %{docs_refs: ["Mod.!/1"]}) ===
               "[`Mod.!/1`](Mod.html#!/1 'Mod.!/1')"

      assert project_doc("`Mod.../2`", %{docs_refs: ["Mod.../2"]}) ===
               "[`Mod.../2`](Mod.html#../2 'Mod.../2')"

      assert project_doc("`Mod.--/2`", %{docs_refs: ["Mod.--/2"]}) ===
               "[`Mod.--/2`](Mod.html#--/2 'Mod.--/2')"

      assert project_doc("`Mod.%/2`", %{docs_refs: ["Mod.%/2"]}) ===
               "[`Mod.%/2`](Mod.html#%25/2 'Mod.%/2')"

      assert project_doc("`Mod.<<>>/1`", %{docs_refs: ["Mod.<<>>/1"]}) ===
               "[`Mod.<<>>/1`](Mod.html#%3C%3C%3E%3E/1 'Mod.<<>>/1')"
    end

    test "autolinks callbacks" do
      assert project_doc("`c:Mod.++/2`", %{docs_refs: ["c:Mod.++/2"]}) ===
               "[`Mod.++/2`](Mod.html#c:++/2 'c:Mod.++/2')"
    end

    test "autolinks types" do
      # use the same approach for elixir_functions as for locals
      assert project_doc(
               "`t:MyModule.my_type/0`",
               %{docs_refs: ["t:MyModule.my_type/0"]}
             ) == "[`MyModule.my_type/0`](MyModule.html#t:my_type/0 't:MyModule.my_type/0')"

      assert project_doc(
               "`t:MyModule.my_type`",
               %{docs_refs: ["t:MyModule.my_type/0"]}
             ) == "`t:MyModule.my_type`"
    end

    test "does not autolink undefined Mod.functions" do
      assert project_doc("`Mod.example/1`", %{docs_refs: ["Mod.example/2"]}) == "`Mod.example/1`"
      assert project_doc("`Mod.example/1`", %{}) == "`Mod.example/1`"
    end

    test "does not autolink pre-linked Mod.functions" do
      assert project_doc("[`Mod.example/1`]()", %{docs_refs: ["Mod.example/1"]}) ==
               "[`Mod.example/1`]()"

      assert project_doc("[the `Mod.example/1`]()", %{docs_refs: ["Mod.example/1"]}) ==
               "[the `Mod.example/1`]()"

      assert project_doc("[`Mod.example/1`](foo)", %{docs_refs: ["Mod.example/1"]}) ==
               "[`Mod.example/1`](foo)"

      assert project_doc("[the `Mod.example/1`](foo)", %{docs_refs: ["Mod.example/1"]}) ==
               "[the `Mod.example/1`](foo)"
    end

    test "supports normal links" do
      assert project_doc("`Mod.example/1`", %{docs_refs: ["Mod.example/1"]}) ==
               "[`Mod.example/1`](Mod.html#example/1 'Mod.example/1')"

      assert project_doc("(`Mod.example/1`)", %{docs_refs: ["Mod.example/1"]}) ==
               "([`Mod.example/1`](Mod.html#example/1 'Mod.example/1'))"

      # It ignores links preceded by "]("
      assert project_doc("](`Mod.example/1`)", %{docs_refs: ["Mod.example/1"]}) ==
               "](`Mod.example/1`)"
    end

    test "supports custom links" do
      assert project_doc("[`example`](`Mod.example/1`)", %{docs_refs: ["Mod.example/1"]}) ==
               "[`example`](Mod.html#example/1 'Mod.example/1')"

      assert project_doc("[the `example`](`Mod.example/1`)", %{docs_refs: ["Mod.example/1"]}) ==
               "[the `example`](Mod.html#example/1 'Mod.example/1')"

      assert project_doc("[the `Mod.example/1`](`Mod.example/1`)", %{docs_refs: ["Mod.example/1"]}) ==
               "[the `Mod.example/1`](Mod.html#example/1 'Mod.example/1')"

      assert project_doc("[`callback(a)`](`c:Foo.foo/1`)", %{docs_refs: ["c:Foo.foo/1"]}) ==
               "[`callback(a)`](Foo.html#c:foo/1 'c:Foo.foo/1')"

      assert project_doc("[the `upcase`](`String.upcase/1`)", %{docs_refs: []}) ==
               "[the `upcase`](#{@elixir_docs}elixir/String.html#upcase/1 'String.upcase/1')"

      assert project_doc("[`f`](`Foo.foo/1`), [`f`](`Foo.foo/1`)", %{docs_refs: ["Foo.foo/1"]}) ==
               "[`f`](Foo.html#foo/1 'Foo.foo/1'), [`f`](Foo.html#foo/1 'Foo.foo/1')"

      assert project_doc("[`foo`](`Foo.//2`)", %{docs_refs: ["Foo.//2"]}) ==
               "[`foo`](Foo.html#//2 'Foo.//2')"

      assert project_doc("[`for`](`Kernel.SpecialForms.for/1`)", %{docs_refs: []}) ==
               "[`for`](#{@elixir_docs}elixir/Kernel.SpecialForms.html#for/1 'Kernel.SpecialForms.for/1')"

      assert project_doc("[`for`](`for/1`)", %{docs_refs: []}) ==
               "[`for`](#{@elixir_docs}elixir/Kernel.SpecialForms.html#for/1 'Kernel.SpecialForms.for/1')"

      assert project_doc("[`is_boolean`](`Kernel.is_boolean/1`)", %{docs_refs: []}) ==
               "[`is_boolean`](#{@elixir_docs}elixir/Kernel.html#is_boolean/1 'Kernel.is_boolean/1')"

      assert project_doc("[`is_boolean`](`is_boolean/1`)", %{docs_refs: []}) ==
               "[`is_boolean`](#{@elixir_docs}elixir/Kernel.html#is_boolean/1 'Kernel.is_boolean/1')"

      assert project_doc("[term()](`t:term/0`)", %{docs_refs: []}) ==
               "[term()](#{@elixir_docs}elixir/typespecs.html#built-in-types 'Built-in types — Typespecs')"

      assert project_doc("[term\(\)](`t:term/0`)", %{docs_refs: []}) ==
               "[term\(\)](#{@elixir_docs}elixir/typespecs.html#built-in-types 'Built-in types — Typespecs')"

      assert project_doc("[`term()`](`t:term/0`)", %{docs_refs: []}) ==
               "[`term()`](#{@elixir_docs}elixir/typespecs.html#built-in-types 'Built-in types — Typespecs')"

      assert project_doc("[`term()`](`t:term/0`)", %{docs_refs: []}) ==
               "[`term()`](#{@elixir_docs}elixir/typespecs.html#built-in-types 'Built-in types — Typespecs')"

      assert project_doc("[version](`t:Version.version/0`)", %{docs_refs: ["t:Version.version/0"]}) ==
               "[version](Version.html#t:version/0 't:Version.version/0')"
    end
  end

  describe "elixir modules" do
    test "autolinks modules in docs" do
      assert project_doc("`MyModule`", %{docs_refs: ["MyModule"], module_id: "MyModule"}) ==
               "[`MyModule`](#content 'MyModule module')"

      assert project_doc("`MyModule.Nested`", %{
               docs_refs: ["MyModule.Nested"],
               module_id: "MyModule.Nested"
             }) == "[`MyModule.Nested`](#content 'MyModule.Nested module')"

      assert project_doc("`MyModule.Nested.Deep`", %{
               docs_refs: ["MyModule.Nested.Deep"],
               module_id: "MyModule.Nested.Deep"
             }) == "[`MyModule.Nested.Deep`](#content 'MyModule.Nested.Deep module')"

      assert project_doc("```\nThis is a test.\n```\n\nSee `MyModule`.", %{
               docs_refs: ["MyModule"],
               module_id: "MyModule"
             }) == "```\nThis is a test.\n```\n\nSee [`MyModule`](#content 'MyModule module')."

      assert project_doc(
               "You can use `Kernel` functions/macros without the `Kernel` prefix anywhere",
               %{docs_refs: ["Kernel"], module_id: "Kernel"}
             ) ==
               "You can use [`Kernel`](#content 'Kernel module')" <>
                 " functions/macros without the [`Kernel`](#content 'Kernel module') prefix anywhere"
    end

    test "autolinks modules in elixir" do
      assert project_doc("`String`", %{docs_refs: ["MyModule"], module_id: "MyModule"}) ==
               "[`String`](#{@elixir_docs}elixir/String.html 'String module')"

      assert project_doc("`Mix`", %{docs_refs: ["MyModule"], module_id: "MyModule"}) ==
               "[`Mix`](#{@elixir_docs}mix/Mix.html 'Mix module')"
    end

    test "autolinks dependencies modules" do
      lib_dirs = [{Application.app_dir(:earmark), "#{@elixir_docs}earmark/"}]

      assert project_doc("`Earmark`", %{
               docs_refs: ["MyModule"],
               module_id: "MyModule",
               extension: ".html",
               lib_dirs: lib_dirs
             }) == "[`Earmark`](#{@elixir_docs}earmark/Earmark.html 'Earmark module')"
    end

    test "does not autolink undefined modules" do
      assert project_doc("`MyModule`", %{docs_refs: []}) == "`MyModule`"
      assert project_doc("`MyModule`", %{docs_refs: ["DiffModule"]}) == "`MyModule`"

      assert project_doc("`MyModule.Nested`", %{docs_refs: ["MyModule.DiffNested"]}) ==
               "`MyModule.Nested`"
    end

    test "does not autolink pre-linked modules" do
      assert project_doc("[`Mod`](other.html)", %{docs_refs: ["Mod"]}) == "[`Mod`](other.html)"

      assert project_doc("[the `Mod`](other.html)", %{docs_refs: ["Mod"]}) ==
               "[the `Mod`](other.html)"

      assert project_doc("[the `Mod.Nested`](other.html)", %{docs_refs: ["Mod.Nested"]}) ==
               "[the `Mod.Nested`](other.html)"

      assert project_doc("[in the `Kernel` module](Kernel.html#guards)", %{
               docs_refs: ["Kernel"]
             }) == "[in the `Kernel` module](Kernel.html#guards)"

      assert project_doc("[in the `Kernel` module](Kernel.html#guards)", %{docs_refs: []}) ==
               "[in the `Kernel` module](Kernel.html#guards)"
    end

    test "supports custom links" do
      assert project_doc("[`example`](`Example`)", %{modules_refs: ["Example"]}) ==
               "[`example`](Example.html 'Example module')"

      assert project_doc("[the `example` module](`Example`)", %{modules_refs: ["Example"]}) ==
               "[the `example` module](Example.html 'Example module')"

      assert project_doc("[the `Example` module](`Example`)", %{modules_refs: ["Example"]}) ==
               "[the `Example` module](Example.html 'Example module')"

      assert project_doc("[the `string` module](`String`)", %{modules_refs: []}) ==
               "[the `string` module](#{@elixir_docs}elixir/String.html 'String module')"

      assert project_doc("[the `String` module](`String`)", %{modules_refs: []}) ==
               "[the `String` module](#{@elixir_docs}elixir/String.html 'String module')"
    end
  end

  describe "Mix tasks" do
    test "autolinks built-in tasks" do
      assert project_doc("`mix test`", %{}) ==
               "[`mix test`](#{@elixir_docs}mix/Mix.Tasks.Test.html 'Mix.Tasks.Test')"
    end

    test "autolinks custom tasks" do
      assert project_doc("`mix foo.bar.baz`", %{modules_refs: ["Mix.Tasks.Foo.Bar.Baz"]}) ==
               "[`mix foo.bar.baz`](Mix.Tasks.Foo.Bar.Baz.html 'Mix.Tasks.Foo.Bar.Baz')"
    end

    test "autolinks task in the same module" do
      assert project_doc("`mix foo`", %{
               modules_refs: ["Mix.Tasks.Foo"],
               module_id: "Mix.Tasks.Foo"
             }) == "[`mix foo`](#content 'Mix.Tasks.Foo')"
    end

    test "autolinks tasks from dependencies" do
      lib_dirs = [{Application.app_dir(:nimble_parsec), "#{@elixir_docs}nimble_parsec/"}]

      assert project_doc("`mix nimble_parsec.compile`", %{lib_dirs: lib_dirs}) ==
               "[`mix nimble_parsec.compile`](#{@elixir_docs}nimble_parsec/Mix.Tasks.NimbleParsec.Compile.html 'Mix.Tasks.NimbleParsec.Compile')"
    end

    test "autolinks Hex tasks" do
      assert project_doc("`mix hex.publish`", %{}) ==
               "[`mix hex.publish`](#{@elixir_docs}hex/Mix.Tasks.Hex.Publish.html 'Mix.Tasks.Hex.Publish')"
    end

    test "autolinks task help" do
      assert project_doc("`mix help compile.app`", %{}) ==
               "[`mix help compile.app`](#{@elixir_docs}mix/Mix.Tasks.Compile.App.html 'Mix.Tasks.Compile.App')"
    end

    test "does not autolink task with arguments" do
      assert project_doc("`mix test test/`", %{}) == "`mix test test/`"
    end

    test "does not autolink unknown task" do
      assert project_doc("`mix foo`", %{}) == "`mix foo`"
    end

    test "does not autolink unknown task help" do
      assert project_doc("`mix help foo`", %{}) == "`mix help foo`"
    end
  end

  describe "erlang functions" do
    test "autolinks to erlang functions" do
      assert project_doc("`:erlang.apply/2`", %{}) ==
               "[`:erlang.apply/2`](#{@erlang_docs}erlang.html#apply-2 ':erlang.apply/2')"

      assert project_doc("`:erlang.adler32/2`", %{}) ==
               "[`:erlang.adler32/2`](#{@erlang_docs}erlang.html#adler32-2 ':erlang.adler32/2')"

      assert project_doc("`:erlang.apply/2` `:erlang.apply/3`", %{}) ==
               "[`:erlang.apply/2`](#{@erlang_docs}erlang.html#apply-2 ':erlang.apply/2') " <>
                 "[`:erlang.apply/3`](#{@erlang_docs}erlang.html#apply-3 ':erlang.apply/3')"

      assert project_doc("`:erl_prim_loader.get_file/1`", %{}) ==
               "[`:erl_prim_loader.get_file/1`](#{@erlang_docs}erl_prim_loader.html#get_file-1 ':erl_prim_loader.get_file/1')"

      assert project_doc("`:zlib.deflateInit/2`", %{}) ==
               "[`:zlib.deflateInit/2`](#{@erlang_docs}zlib.html#deflateInit-2 ':zlib.deflateInit/2')"
    end

    test "autolinks to Erlang functions with custom links" do
      assert project_doc("[`example`](`:lists.reverse/1`)", %{}) ==
               "[`example`](#{@erlang_docs}lists.html#reverse-1 ':lists.reverse/1')"

      assert project_doc("[example](`:lists.reverse/1`)", %{}) ==
               "[example](#{@erlang_docs}lists.html#reverse-1 ':lists.reverse/1')"
    end

    test "does not autolink pre-linked docs" do
      assert project_doc("[`:erlang.apply/2`](other.html)", %{}) ==
               "[`:erlang.apply/2`](other.html)"

      assert project_doc("[the `:erlang.apply/2`](other.html)", %{}) ==
               "[the `:erlang.apply/2`](other.html)"

      assert project_doc("[the `:erlang.apply/2` function](`Kernel.apply/2`)", %{}) ==
               "[the `:erlang.apply/2` function](#{@elixir_docs}elixir/Kernel.html#apply/2 'Kernel.apply/2')"

      assert project_doc("[the :erlang.apply/2 function](`Kernel.apply/2`)", %{}) ==
               "[the :erlang.apply/2 function](#{@elixir_docs}elixir/Kernel.html#apply/2 'Kernel.apply/2')"

      assert project_doc("[the `:erlang.apply/2` function](other.html)", %{}) ==
               "[the `:erlang.apply/2` function](other.html)"
    end

    test "does not autolink for functions that aren't part of the erlang distribution" do
      assert project_doc("`:unknown.foo/0`", %{}) == "`:unknown.foo/0`"
    end
  end

  describe "typespecs" do
    test "formats operators" do
      assert Autolink.typespec(quote(do: +foo() :: foo()), [], "Foo", [], []) ==
               ~s[+foo() :: foo()]

      assert Autolink.typespec(quote(do: foo() + foo() :: foo()), [], "Foo", [], []) ==
               ~s[foo() + foo() :: foo()]
    end

    test "strips parens" do
      assert Autolink.typespec(quote(do: foo({}, bar())), [], "Foo", []) == ~s[foo({}, bar())]
    end

    test "autolinks locals" do
      assert Autolink.typespec(quote(do: foo(1)), [foo: 1], "Foo", []) ==
               ~s[<a href="#t:foo/1" title="t:Foo.foo/1">foo</a>(1)]

      assert Autolink.typespec(quote(do: bar(foo(1))), [foo: 1], "Foo", []) ==
               ~s[bar(<a href="#t:foo/1" title="t:Foo.foo/1">foo</a>(1))]

      assert Autolink.typespec(quote(do: (bar(foo(1)) when bat: foo(1))), [foo: 1], "Foo", []) ==
               ~s[bar(<a href="#t:foo/1" title="t:Foo.foo/1">foo</a>(1)) when bat: <a href=\"#t:foo/1\" title="t:Foo.foo/1">foo</a>(1)]

      assert Autolink.typespec(quote(do: bar(foo(1))), [], "Foo", []) == ~s[bar(foo(1))]
    end

    test "autolinks same type and function name" do
      assert Autolink.typespec(quote(do: foo() :: foo()), [foo: 0], "Foo", [], []) ==
               ~s[foo() :: <a href="#t:foo/0" title="t:Foo.foo/0">foo</a>()]

      assert Autolink.typespec(quote(do: foo(1) :: foo(1)), [foo: 1], "Foo", [], []) ==
               ~s[foo(1) :: <a href="#t:foo/1" title="t:Foo.foo/1">foo</a>(1)]

      assert Autolink.typespec(
               quote(do: (foo(1) :: foo(1) when bat: foo(1))),
               [foo: 1],
               "Foo",
               [],
               []
             ) ==
               ~s[foo(1) :: <a href=\"#t:foo/1\" title=\"t:Foo.foo/1\">foo</a>(1) when bat: <a href=\"#t:foo/1\" title=\"t:Foo.foo/1\">foo</a>(1)]

      assert Autolink.typespec(quote(do: bar(foo(1)) :: foo(1)), [foo: 1], "Foo", [], []) ==
               ~s[bar(<a href=\"#t:foo/1\" title=\"t:Foo.foo/1\">foo</a>(1)) :: <a href=\"#t:foo/1\" title=\"t:Foo.foo/1\">foo</a>(1)]

      assert Autolink.typespec(
               quote(do: (bar(f(1)) :: f(1) when bat: f(1))),
               [f: 1],
               "Foo",
               [],
               []
             ) ==
               ~s[bar(<a href=\"#t:f/1\" title=\"t:Foo.f/1\">f</a>(1)) :: <a href=\"#t:f/1\" title=\"t:Foo.f/1\">f</a>(1) when bat: <a href=\"#t:f/1\" title=\"t:Foo.f/1\">f</a>(1)]

      assert Autolink.typespec(quote(do: bar(foo :: foo(1)) :: foo(1)), [foo: 1], "Foo", [], []) ==
               ~s[bar(foo :: <a href=\"#t:foo/1\" title=\"t:Foo.foo/1\">foo</a>(1)) :: <a href=\"#t:foo/1\" title=\"t:Foo.foo/1\">foo</a>(1)]
    end

    test "autolinks Elixir types" do
      assert Autolink.typespec(quote(do: String.t()), [], "Foo", []) ==
               ~s[<a href="#{@elixir_docs}elixir/String.html#t:t/0" title=\"t:String.t/0\">String.t</a>()]

      assert Autolink.typespec(quote(do: Unknown.bar()), [], "Foo", []) == ~s[Unknown.bar()]
    end

    test "autolinks Elixir basic types" do
      assert Autolink.typespec(quote(do: atom()), [], "Foo", []) ==
               ~s[<a href=\"#{@elixir_docs}elixir/typespecs.html#basic-types\" title=\"Basic types — Typespecs\">atom</a>()]
    end

    test "autolinks Elixir built-in types" do
      assert Autolink.typespec(quote(do: term()), [], "Foo", []) ==
               ~s[<a href=\"#{@elixir_docs}elixir/typespecs.html#built-in-types\" title="Built-in types — Typespecs">term</a>()]

      assert Autolink.typespec(quote(do: term()), [], "Foo", [Kernel]) ==
               ~s[<a href=\"typespecs.html#built-in-types\" title=\"Built-in types — Typespecs\">term</a>()]
    end

    test "autolinks Erlang types" do
      assert Autolink.typespec(quote(do: :sets.set()), [], "Foo", []) ==
               ~s[<a href=\"#{@erlang_docs}sets.html#type-set\" title=\"sets:set\">:sets.set</a>()]

      assert Autolink.typespec(quote(do: :sets.set(foo())), [], "Foo", []) ==
               ~s[<a href=\"#{@erlang_docs}sets.html#type-set\" title=\"sets:set\">:sets.set</a>(foo())]

      assert Autolink.typespec(quote(do: :sets.set(foo())), [foo: 0], "Foo", []) ==
               ~s[<a href=\"#{@erlang_docs}sets.html#type-set\" title=\"sets:set\">:sets.set</a>(<a href=\"#t:foo/0\" title=\"t:Foo.foo/0\">foo</a>())]
    end

    test "autolinks shared aliases" do
      assert Autolink.typespec(quote(do: Foo.t()), [], "Bar", [Foo]) ==
               ~s[<a href="Foo.html#t:t/0" title="t:Foo.t/0">Foo.t</a>()]
    end

    test "autolinks inside parameterized types" do
      assert Autolink.typespec(quote(do: t(foo())), [t: 1, foo: 0], "Foo", []) ==
               ~s[<a href="#t:t/1" title="t:Foo.t/1">t</a>(<a href="#t:foo/0" title="t:Foo.foo/0">foo</a>())]

      assert Autolink.typespec(quote(do: Parameterized.t(foo())), [foo: 0], "Foo", [Parameterized]) ==
               ~s[<a href="Parameterized.html#t:t/1" title="t:Parameterized.t/1">Parameterized.t</a>(<a href="#t:foo/0" title="t:Foo.foo/0">foo</a>())]

      assert Autolink.typespec(quote(do: parameterized_t(Foo.t())), [parameterized_t: 1], "Bar", [
               Foo
             ]) ==
               ~s[<a href="#t:parameterized_t/1" title="t:Bar.parameterized_t/1">parameterized_t</a>(<a href="Foo.html#t:t/0" title="t:Foo.t/0">Foo.t</a>())]

      assert Autolink.typespec(quote(do: Parameterized.t(Foo.t())), [], "Bar", [
               Parameterized,
               Foo
             ]) ==
               ~s[<a href="Parameterized.html#t:t/1" title="t:Parameterized.t/1">Parameterized.t</a>(<a href="Foo.html#t:t/0" title="t:Foo.t/0">Foo.t</a>())]

      assert Autolink.typespec(quote(do: t(foo() | bar())), [t: 1, foo: 0, bar: 0], "Foo", []) ==
               ~s[<a href="#t:t/1" title="t:Foo.t/1">t</a>(<a href="#t:foo/0" title="t:Foo.foo/0">foo</a>() | <a href="#t:bar/0" title="t:Foo.bar/0">bar</a>())]

      assert Autolink.typespec(quote(do: t(t(foo()))), [t: 1, foo: 0], "Foo", []) ==
               ~s[<a href="#t:t/1" title="t:Foo.t/1">t</a>(<a href="#t:t/1" title="t:Foo.t/1">t</a>(<a href="#t:foo/0" title="t:Foo.foo/0">foo</a>()))]

      assert Autolink.typespec(quote(do: parameterized_t(foo())), [foo: 0], "Foo", []) ==
               ~s[parameterized_t(<a href="#t:foo/0" title="t:Foo.foo/0">foo</a>())]

      assert Autolink.typespec(quote(do: parameterized_t(atom())), [], "Foo", []) ==
               ~s[parameterized_t(<a href=\"#{@elixir_docs}elixir/typespecs.html#basic-types\" title=\"Basic types — Typespecs\">atom</a>())]

      assert Autolink.typespec(
               quote(do: parameterized_t(atom()) :: list(function())),
               [],
               "Foo",
               []
             ) ==
               ~s[parameterized_t(<a href=\"#{@elixir_docs}elixir/typespecs.html#basic-types\" title=\"Basic types — Typespecs\">atom</a>()) :: ] <>
                 ~s[<a href=\"#{@elixir_docs}elixir/typespecs.html#basic-types\" title=\"Basic types — Typespecs\">list</a>(] <>
                 ~s[<a href=\"#{@elixir_docs}elixir/typespecs.html#built-in-types\" title=\"Built-in types — Typespecs\">function</a>())]
    end

    test "placeholders" do
      assert_typespec_placeholders(
        "t()",
        "_p1_()",
        [t: 0],
        ""
      )

      assert_typespec_placeholders(
        "foobar()",
        "_ppp1_()",
        [foobar: 0],
        ""
      )

      assert_typespec_placeholders(
        "Mod.foobar()",
        "_ppppppp1_()",
        [],
        "",
        [Mod]
      )

      assert_typespec_placeholders(
        "foobar(barbaz())",
        "_ppp1_(_ppp2_())",
        [foobar: 1, barbaz: 0],
        ""
      )

      assert_typespec_placeholders(
        "Mod.foobar(Mod.barbaz())",
        "_ppppppp1_(_ppppppp2_())",
        [],
        "",
        [Mod]
      )

      assert_typespec_placeholders(
        "foobar(foobar(barbaz()))",
        "_ppp1_(_ppp1_(_ppp2_()))",
        [foobar: 1, barbaz: 0],
        ""
      )
    end
  end

  describe "corner-cases" do
    test "accepts functions around () and []" do
      assert project_doc("`===/2`", %{aliases: [Kernel]}) ===
               "[`===/2`](Kernel.html#===/2 'Kernel.===/2')"

      assert project_doc("(`===/2`)", %{aliases: [Kernel]}) ===
               "([`===/2`](Kernel.html#===/2 'Kernel.===/2'))"

      assert project_doc("[`===/2`]", %{aliases: [Kernel]}) ===
               "[[`===/2`](Kernel.html#===/2 'Kernel.===/2')]"
    end
  end

  ##  Helpers

  defp assert_typespec_placeholders(original, expected, typespecs, module_name, aliases \\ []) do
    ast = Code.string_to_quoted!(original)

    {actual, _} =
      Autolink.format_and_extract_typespec_placeholders(ast, typespecs, module_name, aliases, [])

    assert actual == expected, "Original: #{original}\nExpected: #{expected}\nActual:   #{actual}"
  end

  defp project_doc(string, id \\ nil, compiled) do
    Autolink.project_doc(string, id, compiled)
  end
end
