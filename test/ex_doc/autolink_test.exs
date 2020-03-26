defmodule ExDoc.AutolinkTest do
  use ExUnit.Case, async: true
  doctest ExDoc.Autolink

  defp sigil_t(text, []) do
    {:code, [], [text]}
  end

  setup do
    ExDoc.Refs.clear()
    :ok
  end

  describe "doc/3" do
    test "elixir stdlib module" do
      assert autolinked(~t"String") == "https://hexdocs.pm/elixir/String.html"
      assert autolinked(~t"Elixir.String") == "https://hexdocs.pm/elixir/String.html"
    end

    test "private module" do
      assert_unchanged(~t"String.Unicode")
    end

    test "erlang module" do
      assert_unchanged(~t":array")
    end

    test "unknown module" do
      assert_unchanged(~t"Unknown")
      assert_unchanged(~t":unknown")
    end

    test "project-local module" do
      ExDoc.Refs.insert([
        {{:module, Foo}, true}
      ])

      assert autolinked(~t"Foo") == "Foo.html"
      assert autolinked(~t"String", app: :elixir) == "String.html"
      assert autolinked(~t"Foo", current_module: Foo) == "#content"
    end

    test "remote function" do
      ExDoc.Refs.insert([
        {{:module, Foo}, true},
        {{:function, Foo, :foo, 1}, true}
      ])

      assert autolinked(~t"Foo.foo/1") == "Foo.html#foo/1"

      assert_unchanged(~t"Bad.bar/1")
    end

    test "elixir stdlib function" do
      assert autolinked(~t"String.upcase/2") ==
               "https://hexdocs.pm/elixir/String.html#upcase/2"
    end

    test "elixir function with default argument" do
      assert autolinked(~t"Enum.join/1") == "https://hexdocs.pm/elixir/Enum.html#join/1"
    end

    test "erlang stdlib function" do
      assert autolinked(~t":lists.all/2") ==
               "http://www.erlang.org/doc/man/lists.html#all-2"
    end

    test "local function" do
      ExDoc.Refs.insert([
        {{:module, Foo}, true},
        {{:function, Foo, :foo, 1}, true}
      ])

      assert autolinked(~t"foo/1", current_module: Foo) == "#foo/1"
      assert_unchanged(~t"bar/1", current_module: Foo)
    end

    test "elixir callback" do
      assert autolinked(~t"c:GenServer.handle_call/3") ==
               "https://hexdocs.pm/elixir/GenServer.html#c:handle_call/3"
    end

    # TODO: enable when OTP 23.0-rc2 is out (it should have callbacks support)
    # test "erlang callback" do
    #   assert autolinked(~t"c::gen_server.handle_call/3") ==
    #            "http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3"
    # end

    test "elixir type" do
      assert autolinked(~t"t:Calendar.date/0") ==
               "https://hexdocs.pm/elixir/Calendar.html#t:date/0"
    end

    test "elixir basic & built-in types" do
      assert autolinked(~t"t:atom/0") ==
               "https://hexdocs.pm/elixir/typespecs.html#basic-types"

      assert autolinked(~t"t:keyword/0") ==
               "https://hexdocs.pm/elixir/typespecs.html#built-in-types"

      assert autolinked(~t"t:keyword/0", app: :elixir) ==
               "typespecs.html#built-in-types"
    end

    test "erlang type" do
      assert autolinked(~t"t::array.array/0") ==
               "http://www.erlang.org/doc/man/array.html#type-array"
    end

    test "special forms" do
      assert autolinked(~t"__block__/1", current_module: Kernel.SpecialForms) ==
               "#__block__/1"

      assert autolinked(~t"__aliases__/1", current_module: Kernel.SpecialForms) ==
               "#__aliases__/1"
    end

    test "escaping" do
      assert autolinked(~t"Kernel.SpecialForms.\"%{}\"/1") ==
               "https://hexdocs.pm/elixir/Kernel.SpecialForms.html#%25%7B%7D/1"

      assert autolinked(~t"Kernel.SpecialForms.{}/1") ==
               "https://hexdocs.pm/elixir/Kernel.SpecialForms.html#%7B%7D/1"

      assert autolinked(~t"Kernel.SpecialForms.\"<<>>\"/1") ==
               "https://hexdocs.pm/elixir/Kernel.SpecialForms.html#%3C%3C%3E%3E/1"
    end

    test "custom link" do
      assert autolinked({:a, [href: "`String`"], ["custom", "text"]}) ==
               "https://hexdocs.pm/elixir/String.html"

      assert autolinked({:a, [href: "`:lists`"], ["custom", "text"]}) ==
               "http://www.erlang.org/doc/man/lists.html"

      assert autolinked({:a, [href: "`:lists.all/2`"], ["custom", "text"]}) ==
               "http://www.erlang.org/doc/man/lists.html#all-2"

      # TODO: with custom links and backticks there are no false positives (you
      #       always mean to link) so we should always warn on mismatches?
      #       Though backticks are markdown specific, is that ok?
      # assert_warn(fn ->
      assert autolinked({:a, [href: "`unknown`"], ["custom", "text"]}) ==
               "`unknown`"
    end

    test "mix task" do
      assert autolinked(~t"mix compile.elixir") ==
               "https://hexdocs.pm/mix/Mix.Tasks.Compile.Elixir.html"

      assert autolinked(~t"mix help compile.elixir") ==
               "https://hexdocs.pm/mix/Mix.Tasks.Compile.Elixir.html"

      assert autolinked(~t"mix help help") ==
               "https://hexdocs.pm/mix/Mix.Tasks.Help.html"

      assert autolinked(~t"mix compile.elixir", app: :mix) ==
               "Mix.Tasks.Compile.Elixir.html"

      assert_unchanged(~t"mix compile.elixir --verbose")

      assert_unchanged(~t"mix unknown.task")
    end

    test "3rd party links" do
      assert autolinked(~t"Earmark.as_ast/2") ==
               "https://hexdocs.pm/earmark/Earmark.html#as_ast/2"

      assert_unchanged(~t":test_module.foo/0")
    end

    test "extras" do
      assert autolinked({:a, [href: "foo.md"], ["Foo"]}, extras: ["foo.md"]) == "foo.html"

      assert autolinked({:a, [href: "foo.md"], ["Foo"]}, extras: ["foo.md"], ext: ".xhtml") ==
               "foo.xhtml"

      assert_unchanged({:a, [href: "foo.md"], ["Foo"]}, extras: [])
    end

    test "other link" do
      assert_unchanged({:a, [href: "foo.html"], [~t"String"]})
      assert_unchanged({:a, [href: "foo.html"], ["custom", "text"]})
    end

    test "other" do
      assert_unchanged(~t"String.upcase() / 2")
      assert_unchanged(~t"String.upcase()/2  ")
      assert_unchanged(~t"  String.upcase()/2")
      assert_unchanged(~t":\"atom\"")
      assert_unchanged(~t"1 + 2")
      assert_unchanged({:p, [], ["hello"]})
    end
  end

  describe "typespec/3" do
    test "operators" do
      assert typespec(quote(do: +foo() :: foo())) == ~s[+foo() :: foo()]

      assert typespec(quote(do: foo() + foo() :: foo())) == ~s[foo() + foo() :: foo()]

      assert typespec(quote(do: -0 :: 0)) == ~s[-0 :: 0]
    end

    test "locals" do
      ExDoc.Refs.insert([
        {{:module, MyModule}, true},
        {{:type, MyModule, :foo, 1}, true},
        {{:type, MyModule, :foo, 2}, true}
      ])

      assert typespec(quote(do: t() :: foo(1))) ==
               ~s[t() :: <a href="#t:foo/1">foo</a>(1)]

      assert typespec(quote(do: t() :: bar(foo(1)))) ==
               ~s[t() :: bar(<a href="#t:foo/1">foo</a>(1))]

      assert typespec(quote(do: (t() :: bar(foo(1)) when bat: foo(1)))) ==
               ~s[t() :: bar(<a href="#t:foo/1">foo</a>(1)) when bat: <a href=\"#t:foo/1\">foo</a>(1)]

      assert typespec(quote(do: t() :: bar(baz(1)))) ==
               ~s[t() :: bar(baz(1))]

      assert typespec(quote(do: t() :: foo(bar(), bar()))) ==
               ~s[t() :: <a href="#t:foo/2">foo</a>(bar(), bar())]
    end

    test "remotes" do
      ExDoc.Refs.insert([
        {{:module, Foo}, true},
        {{:type, Foo, :t, 0}, true}
      ])

      assert typespec(quote(do: t() :: Foo.t())) ==
               ~s[t() :: <a href="Foo.html#t:t/0">Foo.t</a>()]
    end

    test "autolinks same type and function name" do
      ExDoc.Refs.insert([
        {{:module, MyModule}, true},
        {{:type, MyModule, :foo, 0}, true},
        {{:type, MyModule, :foo, 1}, true}
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
      {{:module, Foo}, true},
      {{:function, Foo, :bar, 1}, false},
      {{:type, Foo, :bad, 0}, false}
    ])

    assert_warn(fn ->
      assert_unchanged(~t"Foo.bar/1")
    end)

    assert_warn(fn ->
      assert_unchanged(~t"String.upcase/9")
    end)

    assert_warn(fn ->
      assert_unchanged(~t"c:GenServer.handle_call/9")
    end)

    assert_warn(fn ->
      assert_unchanged(~t"t:Calendar.date/9")
    end)

    assert_warn(fn ->
      assert typespec(quote(do: t() :: Foo.bad())) ==
               ~s[t() :: Foo.bad()]
    end)

    assert_warn(fn ->
      assert typespec(quote(do: t() :: String.bad())) ==
               ~s[t() :: String.bad()]
    end)

    options = [skip_undefined_reference_warnings_on: ["MyModule"], module_id: "MyModule"]
    assert_unchanged(~t"String.upcase/9", options)
  end

  ## Helpers

  @default_options [app: :myapp, current_module: MyModule, module_id: "MyModule"]

  defp autolinked(node, options \\ []) do
    case ExDoc.Autolink.doc(node, Keyword.merge(@default_options, options)) do
      {:a, [href: url], _} when is_binary(url) -> url
      _ -> raise "could not build link for `#{inspect(node)}`"
    end
  end

  defp assert_unchanged(node, options \\ []) do
    assert ExDoc.Autolink.doc(node, Keyword.merge(@default_options, options)) == node
  end

  defp assert_warn(fun) do
    assert ExUnit.CaptureIO.capture_io(:stderr, fun) =~ "documentation references"
  end

  defp typespec(ast, options \\ []) do
    ExDoc.Autolink.typespec(ast, Keyword.merge(@default_options, options))
  end
end
