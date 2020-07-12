defmodule ExDoc.AutolinkTest do
  use ExUnit.Case, async: true
  doctest ExDoc.Autolink
  import ExUnit.CaptureIO

  defp sigil_m(text, []) do
    [{:p, _, [ast]}] = ExDoc.Markdown.to_ast(text, [])
    ast
  end

  setup do
    ExDoc.Refs.clear()
    :ok
  end

  describe "doc/3" do
    test "elixir stdlib module" do
      assert autolink("String") == ~m"[`String`](https://hexdocs.pm/elixir/String.html)"

      assert autolink("Elixir.String") ==
               ~m"[`Elixir.String`](https://hexdocs.pm/elixir/String.html)"
    end

    test "other elixir core module" do
      assert autolink("IEx.Helpers") ==
               ~m"[`IEx.Helpers`](https://hexdocs.pm/iex/IEx.Helpers.html)"
    end

    test "hidden module" do
      captured =
        assert_warn(fn ->
          assert_unchanged("String.Unicode")
        end)

      assert captured =~
               "documentation references module \"String.Unicode\" but it is hidden"
    end

    test "erlang module" do
      assert_unchanged(":array")
    end

    test "unknown module" do
      assert_unchanged("Unknown")
      assert_unchanged(":unknown")
    end

    test "project-local module" do
      ExDoc.Refs.insert([
        {{:module, Foo}, :public}
      ])

      assert autolink("Foo") == ~m"[`Foo`](Foo.html)"
      assert autolink("String", app: :elixir) == ~m"[`String`](String.html)"
      assert autolink("Foo", current_module: Foo) == ~m"[`Foo`](#content)"
    end

    test "remote function" do
      ExDoc.Refs.insert([
        {{:module, Foo}, :public},
        {{:function, Foo, :foo, 1}, :public},
        {{:function, Foo, :., 2}, :public},
        {{:function, Foo, :.., 2}, :public}
      ])

      assert autolink("Foo.foo/1") == ~m"[`Foo.foo/1`](Foo.html#foo/1)"
      assert autolink("Foo../2") == ~m"[`Foo../2`](Foo.html#./2)"
      assert autolink("Foo.../2") == ~m"[`Foo.../2`](Foo.html#../2)"

      assert_unchanged("Bad.bar/1")
    end

    test "elixir stdlib function" do
      assert autolink("String.upcase/2") ==
               ~m"[`String.upcase/2`](https://hexdocs.pm/elixir/String.html#upcase/2)"
    end

    test "elixir function with default argument" do
      assert autolink("Enum.join/1") ==
               ~m"[`Enum.join/1`](https://hexdocs.pm/elixir/Enum.html#join/1)"
    end

    test "erlang stdlib function" do
      assert autolink(":lists.all/2") ==
               ~m"[`:lists.all/2`](http://www.erlang.org/doc/man/lists.html#all-2)"
    end

    test "local function" do
      ExDoc.Refs.insert([
        {{:module, Foo}, :public},
        {{:function, Foo, :foo, 1}, :public},
        {{:function, Foo, :., 2}, :public},
        {{:function, Foo, :.., 2}, :public}
      ])

      assert autolink("foo/1", current_module: Foo) == ~m"[`foo/1`](#foo/1)"
      assert autolink("./2", current_module: Foo) == ~m"[`./2`](#./2)"
      assert autolink("../2", current_module: Foo) == ~m"[`../2`](#../2)"
      assert_unchanged("bar/1", current_module: Foo)
    end

    test "sibling function" do
      assert autolink("Earmark.as_ast/2", siblings: [:earmark]) ==
               ~m"[`Earmark.as_ast/2`](Earmark.html#as_ast/2)"
    end

    test "auto-imported function" do
      assert autolink("+/2") ==
               ~m"[`+/2`](https://hexdocs.pm/elixir/Kernel.html#+/2)"

      assert autolink("for/1") ==
               ~m"[`for/1`](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#for/1)"

      assert autolink("for/1", app: :elixir) ==
               ~m"[`for/1`](Kernel.SpecialForms.html#for/1)"
    end

    test "elixir callback" do
      assert autolink("c:GenServer.handle_call/3") ==
               ~m"[`GenServer.handle_call/3`](https://hexdocs.pm/elixir/GenServer.html#c:handle_call/3)"
    end

    test "erlang callback" do
      assert autolink("c::gen_server.handle_call/3") ==
               ~m"[`:gen_server.handle_call/3`](http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3)"
    end

    test "elixir type" do
      assert autolink("t:Calendar.date/0") ==
               ~m"[`Calendar.date/0`](https://hexdocs.pm/elixir/Calendar.html#t:date/0)"
    end

    test "elixir basic & built-in types" do
      assert autolink("t:atom/0") ==
               ~m"[`atom/0`](https://hexdocs.pm/elixir/typespecs.html#basic-types)"

      assert autolink("t:keyword/0") ==
               ~m"[`keyword/0`](https://hexdocs.pm/elixir/typespecs.html#built-in-types)"

      assert autolink("t:keyword/0", app: :elixir) ==
               ~m"[`keyword/0`](typespecs.html#built-in-types)"
    end

    test "erlang type" do
      assert autolink("t::array.array/0") ==
               ~m"[`:array.array/0`](http://www.erlang.org/doc/man/array.html#type-array)"
    end

    test "special forms" do
      assert autolink("__block__/1", current_module: Kernel.SpecialForms) ==
               ~m"[`__block__/1`](#__block__/1)"

      assert autolink("__aliases__/1", current_module: Kernel.SpecialForms) ==
               ~m"[`__aliases__/1`](#__aliases__/1)"
    end

    test "escaping" do
      assert autolink("Kernel.SpecialForms.%{}/1") ==
               ~m"[`Kernel.SpecialForms.%{}/1`](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#%25%7B%7D/1)"

      assert autolink("Kernel.SpecialForms.{}/1") ==
               ~m"[`Kernel.SpecialForms.{}/1`](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#%7B%7D/1)"

      assert autolink("Kernel.SpecialForms.<<>>/1") ==
               ~m"[`Kernel.SpecialForms.<<>>/1`](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#%3C%3C%3E%3E/1)"
    end

    test "custom link" do
      assert autolink(~m"[custom text](`String`)") ==
               ~m"[custom text](https://hexdocs.pm/elixir/String.html)"

      assert autolink(~m"[custom text](`:lists`)") ==
               ~m"[custom text](http://www.erlang.org/doc/man/lists.html)"

      assert autolink(~m"[custom text](`:lists.all/2`)") ==
               ~m"[custom text](http://www.erlang.org/doc/man/lists.html#all-2)"

      captured =
        assert_warn(fn ->
          assert_unchanged(~m"[custom text](`Unknown`)")
        end)

      assert captured =~
               "documentation references module \"Unknown\" but it is undefined"

      captured =
        assert_warn(fn ->
          assert_unchanged(~m"[custom text](Unknown)")
        end)

      assert captured =~
               "documentation references file \"Unknown\" but it doesn't exist"

      captured =
        assert_warn(fn ->
          assert_unchanged(~m"[custom text](`LICENSE`)", extras: ["LICENSE"])
        end)

      assert captured =~
               "documentation references module \"LICENSE\" but it is undefined"

      captured =
        assert_warn(fn ->
          assert_unchanged(~m"[an unknown task](`mix unknown.task`)")
        end)

      assert captured =~
               "documentation references \"mix unknown.task\" but such task is undefined"
    end

    test "mix task" do
      assert autolink("mix compile.elixir") ==
               ~m"[`mix compile.elixir`](https://hexdocs.pm/mix/Mix.Tasks.Compile.Elixir.html)"

      assert autolink("mix help compile.elixir") ==
               ~m"[`mix help compile.elixir`](https://hexdocs.pm/mix/Mix.Tasks.Compile.Elixir.html)"

      assert autolink("mix help help") ==
               ~m"[`mix help help`](https://hexdocs.pm/mix/Mix.Tasks.Help.html)"

      assert autolink("mix compile.elixir", app: :mix) ==
               ~m"[`mix compile.elixir`](Mix.Tasks.Compile.Elixir.html)"

      assert_unchanged("mix compile.elixir --verbose")

      assert_unchanged("mix unknown.task")
    end

    test "3rd party links" do
      assert autolink("Earmark.as_ast/2") ==
               ~m"[`Earmark.as_ast/2`](https://hexdocs.pm/earmark/Earmark.html#as_ast/2)"

      assert_unchanged(":test_module.foo/0")
    end

    test "extras" do
      opts = [extras: ["Foo Bar.md"]]

      assert autolink(~m"[Foo](Foo Bar.md)", opts) == ~m"[Foo](foo-bar.html)"

      assert autolink(~m"[Foo](Foo Bar.md)", [ext: ".xhtml"] ++ opts) == ~m"[Foo](foo-bar.xhtml)"

      assert autolink(~m"[Foo](Foo Bar.md#baz)", opts) == ~m"[Foo](foo-bar.html#baz)"

      assert autolink(~m"[Foo](../guide/Foo Bar.md)", opts) == ~m"[Foo](foo-bar.html)"

      assert_unchanged(~m"[Foo](http://example.com/foo.md)", opts)

      assert_unchanged(~m"[Foo](#baz)", opts)
    end

    test "other link" do
      assert_unchanged(~m"[`String`](foo.html)")
      assert_unchanged(~m"[custom text](foo.html)")
    end

    test "other" do
      assert_unchanged("String.upcase() / 2")
      assert_unchanged("String.upcase()/2  ")
      assert_unchanged("  String.upcase()/2")
      assert_unchanged(":\"atom\"")
      assert_unchanged("1 + 2")
      assert_unchanged({:p, [], ["hello"]})
    end
  end

  describe "typespec/3" do
    test "operators" do
      ExDoc.Refs.insert([
        {{:module, MyModule}, :public},
        {{:type, MyModule, :foo, 0}, :public}
      ])

      assert typespec(quote(do: +foo() :: foo())) ==
               ~s[+<a href="#t:foo/0">foo</a>() :: <a href="#t:foo/0">foo</a>()]

      assert typespec(quote(do: foo() + foo() :: foo())) ==
               ~s[<a href=\"#t:foo/0\">foo</a>() + <a href=\"#t:foo/0\">foo</a>() :: <a href=\"#t:foo/0\">foo</a>()]

      assert typespec(quote(do: -0 :: 0)) == ~s[-0 :: 0]
    end

    test "locals" do
      ExDoc.Refs.insert([
        {{:module, MyModule}, :public},
        {{:type, MyModule, :foo, 1}, :public},
        {{:type, MyModule, :foo, 2}, :public},
        {{:type, MyModule, :foo!, 1}, :public},
        {{:type, MyModule, :bar, 0}, :public},
        {{:type, MyModule, :bar, 1}, :public},
        {{:type, MyModule, :baz, 1}, :public}
      ])

      assert typespec(quote(do: t() :: foo(1))) ==
               ~s[t() :: <a href="#t:foo/1">foo</a>(1)]

      assert typespec(quote(do: t() :: bar(foo(1)))) ==
               ~s[t() :: <a href=\"#t:bar/1\">bar</a>(<a href=\"#t:foo/1\">foo</a>(1))]

      assert typespec(quote(do: (t() :: bar(foo(1)) when bat: foo(1)))) ==
               ~s[t() :: <a href=\"#t:bar/1\">bar</a>(<a href="#t:foo/1">foo</a>(1)) when bat: <a href=\"#t:foo/1\">foo</a>(1)]

      assert typespec(quote(do: t() :: bar(baz(1)))) ==
               ~s[t() :: <a href=\"#t:bar/1\">bar</a>(<a href=\"#t:baz/1\">baz</a>(1))]

      assert typespec(quote(do: t() :: foo(bar(), bar()))) ==
               ~s[t() :: <a href="#t:foo/2">foo</a>(<a href=\"#t:bar/0\">bar</a>(), <a href=\"#t:bar/0\">bar</a>())]

      assert typespec(quote(do: t() :: foo!(bar()))) ==
               ~s[t() :: <a href="#t:foo!/1">foo!</a>(<a href=\"#t:bar/0\">bar</a>())]
    end

    test "remotes" do
      ExDoc.Refs.insert([
        {{:module, Foo}, :public},
        {{:type, Foo, :t, 0}, :public}
      ])

      assert typespec(quote(do: t() :: Foo.t())) ==
               ~s[t() :: <a href="Foo.html#t:t/0">Foo.t</a>()]
    end

    test "autolinks same type and function name" do
      ExDoc.Refs.insert([
        {{:module, MyModule}, :public},
        {{:type, MyModule, :foo, 0}, :public},
        {{:type, MyModule, :foo, 1}, :public}
      ])

      assert typespec(quote(do: foo() :: foo()))
      ~s[foo() :: <a href="#t:foo/0">foo</a>()]

      assert typespec(quote(do: foo(1) :: foo(1))) ==
               ~s[foo(1) :: <a href="#t:foo/1">foo</a>(1)]

      assert typespec(quote(do: (foo(1) :: foo(1) when bat: foo(1)))) ==
               ~s[foo(1) :: <a href=\"#t:foo/1\">foo</a>(1) when bat: <a href=\"#t:foo/1\">foo</a>(1)]

      assert typespec(quote(do: bar(foo(1)) :: foo(1))) ==
               ~s[bar(<a href=\"#t:foo/1\">foo</a>(1)) :: <a href=\"#t:foo/1\">foo</a>(1)]

      assert typespec(quote(do: (bar(foo(1)) :: foo(1) when bat: foo(1)))) ==
               ~s[bar(<a href=\"#t:foo/1\">foo</a>(1)) :: <a href=\"#t:foo/1\">foo</a>(1) when bat: <a href=\"#t:foo/1\">foo</a>(1)]

      assert typespec(quote(do: bar(foo :: foo(1)) :: foo(1))) ==
               ~s[bar(foo :: <a href=\"#t:foo/1\">foo</a>(1)) :: <a href=\"#t:foo/1\">foo</a>(1)]
    end

    test "Elixir stdlib types" do
      assert typespec(quote(do: t() :: String.t())) ==
               ~s[t() :: <a href="https://hexdocs.pm/elixir/String.html#t:t/0">String.t</a>()]
    end

    test "Elixir basic types" do
      assert typespec(quote(do: t() :: atom())) ==
               ~s[t() :: <a href=\"https://hexdocs.pm/elixir/typespecs.html#basic-types\">atom</a>()]
    end

    test "Elixir built-in types" do
      assert typespec(quote(do: t() :: keyword())) ==
               ~s[t() :: <a href=\"https://hexdocs.pm/elixir/typespecs.html#built-in-types\">keyword</a>()]
    end

    test "Erlang stdlib types" do
      assert typespec(quote(do: t() :: :sets.set())) ==
               ~s[t() :: <a href=\"http://www.erlang.org/doc/man/sets.html#type-set\">:sets.set</a>()]
    end
  end

  test "warnings" do
    ExDoc.Refs.insert([
      {{:module, Foo}, :public},
      {{:function, Foo, :bar, 1}, :hidden},
      {{:type, Foo, :bad, 0}, :hidden}
    ])

    captured =
      assert_warn(fn ->
        assert_unchanged("Foo.bar/1", file: "lib/foo.ex", line: 1, id: nil)
      end)

    assert captured =~ "documentation references function \"Foo.bar/1\""
    assert captured =~ ~r{lib/foo.ex:1\n$}

    captured =
      assert_warn(fn ->
        assert_unchanged("Foo.bar/1", file: "lib/foo.ex", id: "Foo.foo/0")
      end)

    assert captured =~ "documentation references function \"Foo.bar/1\""
    assert captured =~ ~r{lib/foo.ex: Foo.foo/0\n$}

    assert_warn(fn ->
      assert_unchanged("String.upcase/9")
    end)

    assert_warn(fn ->
      assert_unchanged("c:GenServer.handle_call/9")
    end)

    assert_warn(fn ->
      assert_unchanged("t:Calendar.date/9")
    end)

    assert_warn(fn ->
      assert typespec(quote(do: t() :: Foo.bad())) ==
               ~s[t() :: Foo.bad()]
    end)

    assert_warn(fn ->
      assert typespec(quote(do: t() :: String.bad())) ==
               ~s[t() :: String.bad()]
    end)

    captured =
      assert_warn(fn ->
        opts = [extras: []]
        assert_unchanged(~m"[Foo](Foo Bar.md)", opts)
      end)

    assert captured =~ "documentation references file \"Foo Bar.md\" but it doesn't exist"

    options = [skip_undefined_reference_warnings_on: ["MyModule"], module_id: "MyModule"]
    assert_unchanged("String.upcase/9", options)

    captured =
      assert_warn(fn ->
        opts = [extras: []]
        assert_unchanged(~m"[bad](`String.upcase/9`)", opts)
      end)

    assert captured =~
             "documentation references function \"String.upcase/9\" but it is undefined"

    captured =
      assert_warn(fn ->
        assert_unchanged(~m"[Unknown](`Unknown`)")
      end)

    assert captured =~
             "documentation references module \"Unknown\" but it is undefined"
  end

  ## Helpers

  @default_options [app: :myapp, current_module: MyModule, module_id: "MyModule", file: "nofile"]

  defp autolink(ast_or_text, options \\ []) do
    ExDoc.Autolink.doc(ast(ast_or_text), Keyword.merge(@default_options, options))
  end

  defp assert_unchanged(ast_or_text, options \\ []) do
    assert autolink(ast_or_text, options) == ast(ast_or_text)
  end

  defp ast(text) when is_binary(text), do: {:code, [class: "inline"], [text]}
  defp ast({_, _, _} = ast), do: ast

  defp assert_warn(fun) do
    captured = capture_io(:stderr, fun)
    captured =~ "documentation references"
    captured
  end

  defp typespec(ast, options \\ []) do
    ExDoc.Autolink.typespec(ast, Keyword.merge(@default_options, options))
  end
end
