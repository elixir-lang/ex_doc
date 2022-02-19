defmodule ExDoc.Language.ElixirTest do
  use ExUnit.Case, async: true
  doctest ExDoc.Autolink
  import ExUnit.CaptureIO

  defp sigil_m(text, []) do
    [{:p, _, [ast], _}] = ExDoc.Markdown.to_ast(text, [])
    ast
  end

  setup do
    ExDoc.Refs.clear()
    :ok
  end

  describe "autolink_doc/2" do
    test "elixir stdlib module" do
      assert autolink_doc("String") == ~m"[`String`](https://hexdocs.pm/elixir/String.html)"

      assert autolink_doc("Elixir.String") ==
               ~m"[`Elixir.String`](https://hexdocs.pm/elixir/String.html)"
    end

    test "other elixir core module" do
      assert autolink_doc("IEx.Helpers") ==
               ~m"[`IEx.Helpers`](https://hexdocs.pm/iex/IEx.Helpers.html)"
    end

    test "erlang module" do
      assert_unchanged(":array")
    end

    test "unknown module" do
      assert_unchanged("Unknown")
      assert_unchanged(":unknown")
      assert_unchanged("A.b.C")
    end

    test "project-local module" do
      ExDoc.Refs.insert([
        {{:module, AutolinkTest.Foo}, :public}
      ])

      assert autolink_doc("AutolinkTest.Foo") == ~m"[`AutolinkTest.Foo`](AutolinkTest.Foo.html)"
      assert autolink_doc("String", apps: [:elixir]) == ~m"[`String`](String.html)"

      assert autolink_doc("AutolinkTest.Foo", current_module: AutolinkTest.Foo) ==
               ~m"[`AutolinkTest.Foo`](AutolinkTest.Foo.html#content)"
    end

    test "remote function" do
      ExDoc.Refs.insert([
        {{:module, AutolinkTest.Foo}, :public},
        {{:function, AutolinkTest.Foo, :foo, 1}, :public},
        {{:function, AutolinkTest.Foo, :., 2}, :public},
        {{:function, AutolinkTest.Foo, :.., 2}, :public}
      ])

      assert autolink_doc("AutolinkTest.Foo.foo/1") ==
               ~m"[`AutolinkTest.Foo.foo/1`](AutolinkTest.Foo.html#foo/1)"

      assert autolink_doc("AutolinkTest.Foo../2") ==
               ~m"[`AutolinkTest.Foo../2`](AutolinkTest.Foo.html#./2)"

      assert autolink_doc("AutolinkTest.Foo.../2") ==
               ~m"[`AutolinkTest.Foo.../2`](AutolinkTest.Foo.html#../2)"

      assert_unchanged("AutolinkTest.Bad.bar/1")
    end

    test "elixir stdlib function" do
      assert autolink_doc("String.upcase/2") ==
               ~m"[`String.upcase/2`](https://hexdocs.pm/elixir/String.html#upcase/2)"
    end

    test "elixir function with default argument" do
      assert autolink_doc("Enum.join/1") ==
               ~m"[`Enum.join/1`](https://hexdocs.pm/elixir/Enum.html#join/1)"
    end

    test "erlang stdlib function" do
      assert autolink_doc(":lists.all/2") ==
               ~m"[`:lists.all/2`](https://www.erlang.org/doc/man/lists.html#all-2)"
    end

    test "local function" do
      ExDoc.Refs.insert([
        {{:module, AutolinkTest.Foo}, :public},
        {{:function, AutolinkTest.Foo, :foo, 1}, :public},
        {{:function, AutolinkTest.Foo, :., 2}, :public},
        {{:function, AutolinkTest.Foo, :.., 2}, :public}
      ])

      assert autolink_doc("foo/1", current_module: AutolinkTest.Foo) == ~m"[`foo/1`](#foo/1)"
      assert autolink_doc("./2", current_module: AutolinkTest.Foo) == ~m"[`./2`](#./2)"
      assert autolink_doc("../2", current_module: AutolinkTest.Foo) == ~m"[`../2`](#../2)"
      assert_unchanged("bar/1", current_module: AutolinkTest.Foo)
    end

    test "auto-imported function" do
      assert autolink_doc("+/2") ==
               ~m"[`+/2`](https://hexdocs.pm/elixir/Kernel.html#+/2)"

      assert autolink_doc("&/1") ==
               ~m"[`&/1`](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#&/1)"

      assert autolink_doc("for/1") ==
               ~m"[`for/1`](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#for/1)"

      assert autolink_doc("for/1", apps: [:elixir]) ==
               ~m"[`for/1`](Kernel.SpecialForms.html#for/1)"
    end

    @tag skip: not Version.match?(System.version(), "~> 1.13")
    test "stepped range" do
      assert autolink_doc("..///3") ==
               ~m"[`..///3`](https://hexdocs.pm/elixir/Kernel.html#..///3)"
    end

    test "elixir callback" do
      assert autolink_doc("c:GenServer.handle_call/3") ==
               ~m"[`GenServer.handle_call/3`](https://hexdocs.pm/elixir/GenServer.html#c:handle_call/3)"
    end

    test "erlang callback" do
      assert autolink_doc("c::gen_server.handle_call/3") ==
               ~m"[`:gen_server.handle_call/3`](https://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3)"
    end

    test "elixir type" do
      assert autolink_doc("t:Calendar.date/0") ==
               ~m"[`Calendar.date/0`](https://hexdocs.pm/elixir/Calendar.html#t:date/0)"
    end

    test "elixir basic & built-in types" do
      assert autolink_doc("t:atom/0") ==
               ~m"[`atom/0`](https://hexdocs.pm/elixir/typespecs.html#basic-types)"

      assert autolink_doc("t:keyword/0") ==
               ~m"[`keyword/0`](https://hexdocs.pm/elixir/typespecs.html#built-in-types)"

      assert autolink_doc("t:keyword/0", apps: [:elixir]) ==
               ~m"[`keyword/0`](typespecs.html#built-in-types)"
    end

    test "erlang type" do
      assert autolink_doc("t::array.array/0") ==
               ~m"[`:array.array/0`](https://www.erlang.org/doc/man/array.html#type-array)"
    end

    test "special forms" do
      assert autolink_doc("__block__/1", current_module: Kernel.SpecialForms) ==
               ~m"[`__block__/1`](#__block__/1)"

      assert autolink_doc("__aliases__/1", current_module: Kernel.SpecialForms) ==
               ~m"[`__aliases__/1`](#__aliases__/1)"
    end

    test "escaping" do
      assert autolink_doc("Kernel.SpecialForms.%{}/1") ==
               ~m"[`Kernel.SpecialForms.%{}/1`](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#%25%7B%7D/1)"

      assert autolink_doc("Kernel.SpecialForms.{}/1") ==
               ~m"[`Kernel.SpecialForms.{}/1`](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#%7B%7D/1)"

      assert autolink_doc("Kernel.SpecialForms.<<>>/1") ==
               ~m"[`Kernel.SpecialForms.<<>>/1`](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#%3C%3C%3E%3E/1)"
    end

    test "custom link" do
      assert autolink_doc(~m"[custom text](`String`)") ==
               ~m"[custom text](https://hexdocs.pm/elixir/String.html)"

      assert autolink_doc(~m"[custom text](`String.at/2`)") ==
               ~m"[custom text](https://hexdocs.pm/elixir/String.html#at/2)"

      assert autolink_doc(~m"[custom text](`:lists`)") ==
               ~m"[custom text](https://www.erlang.org/doc/man/lists.html)"

      assert autolink_doc(~m"[custom text](`:lists.all/2`)") ==
               ~m"[custom text](https://www.erlang.org/doc/man/lists.html#all-2)"
    end

    test "mix task" do
      assert autolink_doc("mix compile.elixir") ==
               ~m"[`mix compile.elixir`](https://hexdocs.pm/mix/Mix.Tasks.Compile.Elixir.html)"

      assert autolink_doc("mix help compile.elixir") ==
               ~m"[`mix help compile.elixir`](https://hexdocs.pm/mix/Mix.Tasks.Compile.Elixir.html)"

      assert autolink_doc("mix help help") ==
               ~m"[`mix help help`](https://hexdocs.pm/mix/Mix.Tasks.Help.html)"

      assert autolink_doc("mix compile.elixir", apps: [:mix]) ==
               ~m"[`mix compile.elixir`](Mix.Tasks.Compile.Elixir.html)"

      assert_unchanged("mix compile.elixir --verbose")

      assert_unchanged("mix unknown.task")
    end

    test "3rd party links" do
      assert autolink_doc("EarmarkParser.as_ast/2") ==
               ~m"[`EarmarkParser.as_ast/2`](https://hexdocs.pm/earmark_parser/EarmarkParser.html#as_ast/2)"

      assert autolink_doc("EarmarkParser.as_ast/2", deps: [earmark_parser: "https://example.com/"]) ==
               ~m"[`EarmarkParser.as_ast/2`](https://example.com/EarmarkParser.html#as_ast/2)"

      assert autolink_doc("EarmarkParser.as_ast/2", deps: [earmark_parser: "https://example.com"]) ==
               ~m"[`EarmarkParser.as_ast/2`](https://example.com/EarmarkParser.html#as_ast/2)"

      # extensions are ignored for external links
      assert autolink_doc("EarmarkParser.as_ast/2", ext: ".xhtml") ==
               ~m"[`EarmarkParser.as_ast/2`](https://hexdocs.pm/earmark_parser/EarmarkParser.html#as_ast/2)"
    end

    test "extras" do
      opts = [extras: %{"Foo Bar.md" => "foo-bar"}]

      assert autolink_doc(~m"[Foo](Foo Bar.md)", opts) == ~m"[Foo](foo-bar.html)"

      assert autolink_doc(~m"[Foo](Foo Bar.md)", [ext: ".xhtml"] ++ opts) ==
               ~m"[Foo](foo-bar.xhtml)"

      assert autolink_doc(~m"[Foo](Foo Bar.md#baz)", opts) == ~m"[Foo](foo-bar.html#baz)"

      assert autolink_doc(~m"[Foo](../guide/Foo Bar.md)", opts) == ~m"[Foo](foo-bar.html)"

      assert_unchanged(~m"[Foo](http://example.com/foo.md)", opts)

      assert_unchanged(~m"[Foo](#baz)", opts)
    end

    test "special case links" do
      assert autolink_doc(~m"`//2`") ==
               {:a, [href: "https://hexdocs.pm/elixir/Kernel.html#//2"], [ast("//2")], %{}}

      assert autolink_doc(~m"[division](`//2`)") ==
               {:a, [href: "https://hexdocs.pm/elixir/Kernel.html#//2"], ["division"], %{}}

      assert autolink_doc(~m"`Kernel.//2`") ==
               {:a, [href: "https://hexdocs.pm/elixir/Kernel.html#//2"], [ast("Kernel.//2")], %{}}

      assert autolink_doc(~m"[division](`Kernel.//2`)") ==
               {:a, [href: "https://hexdocs.pm/elixir/Kernel.html#//2"], ["division"], %{}}
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
      assert_unchanged({:p, [], ["hello"], %{}})
    end
  end

  describe "autolink_spec/3" do
    test "operators" do
      ExDoc.Refs.insert([
        {{:module, MyModule}, :public},
        {{:type, MyModule, :foo, 0}, :public}
      ])

      assert autolink_spec(quote(do: +foo() :: foo())) ==
               ~s[+<a href="#t:foo/0">foo</a>() :: <a href="#t:foo/0">foo</a>()]

      assert autolink_spec(quote(do: foo() + foo() :: foo())) ==
               ~s[<a href="#t:foo/0">foo</a>() + <a href="#t:foo/0">foo</a>() :: <a href="#t:foo/0">foo</a>()]

      assert autolink_spec(quote(do: -0 :: 0)) == ~s[-0 :: 0]
    end

    test "locals" do
      ExDoc.Refs.insert([
        {{:module, MyModule}, :public},
        {{:type, MyModule, :foo, 1}, :public},
        {{:type, MyModule, :foo, 2}, :public},
        {{:type, MyModule, :foo?, 1}, :public},
        {{:type, MyModule, :foo!, 1}, :public},
        {{:type, MyModule, :bar, 0}, :public},
        {{:type, MyModule, :bar, 1}, :public},
        {{:type, MyModule, :baz, 1}, :public}
      ])

      assert autolink_spec(quote(do: unquote(:"/=")() :: :ok)) ==
               ~s[/=() :: :ok]

      assert autolink_spec(quote(do: t() :: foo(1))) ==
               ~s[t() :: <a href="#t:foo/1">foo</a>(1)]

      assert autolink_spec(quote(do: t() :: bar(foo(1)))) ==
               ~s[t() :: <a href="#t:bar/1">bar</a>(<a href="#t:foo/1">foo</a>(1))]

      assert autolink_spec(quote(do: (t() :: bar(foo(1)) when bat: foo(1)))) ==
               ~s[t() :: <a href="#t:bar/1">bar</a>(<a href="#t:foo/1">foo</a>(1)) when bat: <a href="#t:foo/1">foo</a>(1)]

      assert autolink_spec(quote(do: t() :: bar(baz(1)))) ==
               ~s[t() :: <a href="#t:bar/1">bar</a>(<a href="#t:baz/1">baz</a>(1))]

      assert autolink_spec(quote(do: t() :: foo(bar(), bar()))) ==
               ~s[t() :: <a href="#t:foo/2">foo</a>(<a href="#t:bar/0">bar</a>(), <a href="#t:bar/0">bar</a>())]

      assert autolink_spec(quote(do: t() :: foo!(bar()))) ==
               ~s[t() :: <a href="#t:foo!/1">foo!</a>(<a href="#t:bar/0">bar</a>())]

      assert autolink_spec(quote(do: t() :: foo?(bar()))) ==
               ~s[t() :: <a href="#t:foo?/1">foo?</a>(<a href="#t:bar/0">bar</a>())]

      assert autolink_spec(
               quote do
                 t() :: %{
                   required(bar()) => bar(),
                   optional(bar()) => bar()
                 }
               end
             ) ==
               ~s[t() :: %{required(<a href="#t:bar/0">bar</a>()) =&gt; <a href="#t:bar/0">bar</a>(), optional(<a href="#t:bar/0">bar</a>()) =&gt; <a href="#t:bar/0">bar</a>()}]
    end

    test "remotes" do
      ExDoc.Refs.insert([
        {{:module, AutolinkTest.Foo}, :public},
        {{:type, AutolinkTest.Foo, :t, 0}, :public}
      ])

      assert autolink_spec(quote(do: t() :: AutolinkTest.Foo.t())) ==
               ~s[t() :: <a href="AutolinkTest.Foo.html#t:t/0">AutolinkTest.Foo.t</a>()]
    end

    test "skip typespec name" do
      ExDoc.Refs.insert([
        {{:module, MyModule}, :public},
        {{:type, MyModule, :foo, 0}, :public},
        {{:type, MyModule, :foo, 1}, :public}
      ])

      assert autolink_spec(quote(do: foo() :: foo()))
      ~s[foo() :: <a href="#t:foo/0">foo</a>()]

      assert autolink_spec(quote(do: foo(1) :: foo(1))) ==
               ~s[foo(1) :: <a href="#t:foo/1">foo</a>(1)]

      assert autolink_spec(quote(do: (foo(1) :: foo(1) when bat: foo(1)))) ==
               ~s[foo(1) :: <a href="#t:foo/1">foo</a>(1) when bat: <a href="#t:foo/1">foo</a>(1)]

      assert autolink_spec(quote(do: bar(foo(1)) :: foo(1))) ==
               ~s[bar(<a href="#t:foo/1">foo</a>(1)) :: <a href="#t:foo/1">foo</a>(1)]

      assert autolink_spec(quote(do: (bar(foo(1)) :: foo(1) when bat: foo(1)))) ==
               ~s[bar(<a href="#t:foo/1">foo</a>(1)) :: <a href="#t:foo/1">foo</a>(1) when bat: <a href="#t:foo/1">foo</a>(1)]

      assert autolink_spec(quote(do: bar(foo :: foo(1)) :: foo(1))) ==
               ~s[bar(foo :: <a href="#t:foo/1">foo</a>(1)) :: <a href="#t:foo/1">foo</a>(1)]
    end

    test "Elixir stdlib types" do
      assert autolink_spec(quote(do: t() :: String.t())) ==
               ~s[t() :: <a href="https://hexdocs.pm/elixir/String.html#t:t/0">String.t</a>()]
    end

    test "Elixir basic types" do
      assert autolink_spec(quote(do: t() :: atom())) ==
               ~s[t() :: <a href="https://hexdocs.pm/elixir/typespecs.html#basic-types">atom</a>()]
    end

    test "Elixir built-in types" do
      assert autolink_spec(quote(do: t() :: keyword())) ==
               ~s[t() :: <a href="https://hexdocs.pm/elixir/typespecs.html#built-in-types">keyword</a>()]
    end

    test "Erlang stdlib types" do
      assert autolink_spec(quote(do: t() :: :sets.set())) ==
               ~s[t() :: <a href="https://www.erlang.org/doc/man/sets.html#type-set">:sets.set</a>()]
    end

    test "escape special HTML characters" do
      assert autolink_spec(quote(do: term() < term() :: boolean())) ==
               ~s[<a href="https://hexdocs.pm/elixir/typespecs.html#built-in-types">term</a>() &lt; <a href="https://hexdocs.pm/elixir/typespecs.html#built-in-types">term</a>() :: <a href="https://hexdocs.pm/elixir/typespecs.html#built-in-types">boolean</a>()]
    end

    test "extensions are ignored for external links" do
      assert autolink_spec(quote(do: t() :: String.t()), ext: ".xhtml") ==
               ~s[t() :: <a href="https://hexdocs.pm/elixir/String.html#t:t/0">String.t</a>()]
    end
  end

  defmodule InMemory do
    @callback hello() :: :world
    def hello(), do: :world
  end

  test "in memory" do
    assert {:a, _, _, _} = autolink_doc("ExDoc.Language.ElixirTest.InMemory.hello/0")
    assert {:a, _, _, _} = autolink_doc("c:ExDoc.Language.ElixirTest.InMemory.hello/0")

    # Types are not checked for in memory
    assert_unchanged("t:ExDoc.Language.ElixirTest.InMemory.unknown/0")

    warn("ExDoc.Language.ElixirTest.InMemory.unknown/0")
    warn("c:ExDoc.Language.ElixirTest.InMemory.unknown/0")
  end

  test "warnings" do
    ExDoc.Refs.insert([
      {{:module, AutolinkTest.Foo}, :public},
      {{:function, AutolinkTest.Foo, :bar, 1}, :hidden},
      {{:type, AutolinkTest.Foo, :bad, 0}, :hidden}
    ])

    captured = warn("AutolinkTest.Foo.bar/1", file: "lib/foo.ex", line: 1, id: nil)

    assert captured =~
             ~s[documentation references function "AutolinkTest.Foo.bar/1" but it is hidden\n]

    assert captured =~ ~r{lib/foo.ex:1\n$}

    assert warn("t:AutolinkTest.Foo.bad/0", file: "lib/foo.ex", id: "AutolinkTest.Foo.foo/0") =~
             ~s[documentation references type "t:AutolinkTest.Foo.bad/0" but it is hidden or private]

    assert warn("t:Elixir.AutolinkTest.Foo.bad/0",
             file: "lib/foo.ex",
             id: "AutolinkTest.Foo.foo/0"
           ) =~
             ~s[documentation references type "t:Elixir.AutolinkTest.Foo.bad/0" but it is hidden or private]

    assert warn("t:AutolinkTest.Foo.bad/0", file: "lib/foo.ex", id: "AutolinkTest.Foo.foo/0") =~
             ~s[documentation references type "t:AutolinkTest.Foo.bad/0" but it is hidden or private]

    assert warn("Code.Typespec") =~
             ~s[documentation references module "Code.Typespec" but it is hidden]

    warn("String.upcase/9")

    warn("c:GenServer.handle_call/9")

    warn("t:Calendar.date/9")

    assert warn(fn ->
             autolink_spec(quote(do: t() :: String.bad()))
           end) =~ ~s[documentation references type "String.bad()"]

    assert warn(fn ->
             autolink_spec(
               quote do
                 t() :: %{
                   name: String.bad()
                 }
               end
             )
           end) =~ ~s[documentation references type "String.bad()"]

    assert warn(~m"[Foo](Foo Bar.md)", extras: %{}) =~
             ~s[documentation references file "Foo Bar.md" but it does not exist]

    options = [skip_undefined_reference_warnings_on: ["MyModule"], module_id: "MyModule"]
    assert_unchanged("String.upcase/9", options)

    assert warn(fn ->
             assert autolink_doc(~m"[Bar A](`Bar.A`)") ==
                      ["Bar A"]
           end) =~ ~s[module "Bar.A" but it is undefined]

    assert_unchanged(~m"`Bar.A`")

    assert warn(fn ->
             assert autolink_doc(~m"[custom text](`Elixir.Unknown`)") ==
                      ["custom text"]
           end) =~ ~s[documentation references module "Elixir.Unknown" but it is undefined]

    assert warn(fn ->
             assert autolink_doc(~m"[It is Unknown](`Unknown`)") ==
                      ["It is Unknown"]
           end) =~ ~s[documentation references module "Unknown" but it is undefined]

    assert warn(~m"[Foo task](`mix foo`)", []) =~
             ~s[documentation references "mix foo" but it is undefined]

    assert_unchanged(~m"`mix foo`")

    assert warn(~m"[bad](`String.upcase/9`)", extras: []) =~
             ~s[documentation references function "String.upcase/9" but it is undefined or private]

    assert_unchanged(~m"`Unknown`")

    assert_unchanged(~m"[Blank](about:blank)")
  end

  ## Helpers

  @default_options [
    apps: [:myapp],
    current_module: MyModule,
    module_id: "MyModule",
    file: "nofile"
  ]

  defp autolink_doc(ast_or_text, options \\ []) do
    ExDoc.Language.Elixir.autolink_doc(ast(ast_or_text), Keyword.merge(@default_options, options))
  end

  defp assert_unchanged(ast_or_text, options \\ []) do
    captured =
      capture_io(:stderr, fn ->
        do_assert_unchanged(ast_or_text, options)
      end)

    unless captured == "" do
      IO.puts(captured)
      raise "failing due to warnings"
    end
  end

  defp do_assert_unchanged(ast_or_text, options) do
    assert autolink_doc(ast_or_text, options) == ast(ast_or_text)
  end

  defp ast(text) when is_binary(text), do: {:code, [class: "inline"], [text], %{}}
  defp ast({_, _, _, _} = ast), do: ast

  defp warn(fun) when is_function(fun, 0) do
    captured = capture_io(:stderr, fun)

    case Regex.scan(~r/documentation references/, captured) do
      [_] -> :ok
      items -> raise "got #{length(items)} warnings in:\n\n#{captured}"
    end

    captured
  end

  defp warn(ast_or_text, options \\ []) do
    warn(fn ->
      do_assert_unchanged(ast_or_text, options)
    end)
  end

  defp autolink_spec(ast, options \\ []) do
    ExDoc.Language.Elixir.autolink_spec(ast, Keyword.merge(@default_options, options))
  end
end
