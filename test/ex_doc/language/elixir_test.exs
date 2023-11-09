defmodule ExDoc.Language.ElixirTest do
  # ExDoc.Refs is global
  use ExUnit.Case, async: false

  doctest ExDoc.Autolink

  describe "autolink_doc/2" do
    test "elixir stdlib module" do
      assert autolink_doc("`String`") ==
               ~s|<a href="https://hexdocs.pm/elixir/String.html"><code class="inline">String</code></a>|

      assert autolink_doc("`Elixir.String`") ==
               ~s|<a href="https://hexdocs.pm/elixir/String.html"><code class="inline">Elixir.String</code></a>|
    end

    test "other elixir core module" do
      assert autolink_doc("`IEx.Helpers`") ==
               ~s|<a href="https://hexdocs.pm/iex/IEx.Helpers.html"><code class="inline">IEx.Helpers</code></a>|
    end

    test "case-sensitive module lookup" do
      assert autolink_doc("`Path`") ==
               ~s|<a href="https://hexdocs.pm/elixir/Path.html"><code class="inline">Path</code></a>|

      assert autolink_doc("`PATH`") == ~s|<code class="inline">PATH</code>|
    end

    test "erlang module" do
      assert autolink_doc("`:array`") == ~s|<code class="inline">:array</code>|
    end

    test "unknown module" do
      assert autolink_doc("`Unknown`") == ~s|<code class="inline">Unknown</code>|
      assert autolink_doc("`:unknown`") == ~s|<code class="inline">:unknown</code>|
      assert autolink_doc("`A.b.C`") == ~s|<code class="inline">A.b.C</code>|
    end

    test "project-local module" do
      ExDoc.Refs.insert([
        {{:module, AutolinkTest.Foo}, :public}
      ])

      assert autolink_doc("`AutolinkTest.Foo`") ==
               ~s|<a href="AutolinkTest.Foo.html"><code class="inline">AutolinkTest.Foo</code></a>|

      assert autolink_doc("`m:AutolinkTest.Foo`") ==
               ~s|<a href="AutolinkTest.Foo.html"><code class="inline">AutolinkTest.Foo</code></a>|

      assert autolink_doc("`String`", apps: [:elixir]) ==
               ~s|<a href="String.html"><code class="inline">String</code></a>|

      assert autolink_doc("`AutolinkTest.Foo`", current_module: AutolinkTest.Foo) ==
               ~s|<a href="AutolinkTest.Foo.html#content"><code class="inline">AutolinkTest.Foo</code></a>|
    end

    test "remote function" do
      ExDoc.Refs.insert([
        {{:module, AutolinkTest.Foo}, :public},
        {{:function, AutolinkTest.Foo, :foo, 1}, :public},
        {{:function, AutolinkTest.Foo, :., 2}, :public},
        {{:function, AutolinkTest.Foo, :.., 2}, :public}
      ])

      assert autolink_doc("`AutolinkTest.Foo.foo/1`") ==
               ~s|<a href="AutolinkTest.Foo.html#foo/1"><code class="inline">AutolinkTest.Foo.foo/1</code></a>|

      assert autolink_doc("`AutolinkTest.Foo../2`") ==
               ~s|<a href="AutolinkTest.Foo.html#./2"><code class="inline">AutolinkTest.Foo../2</code></a>|

      assert autolink_doc("`AutolinkTest.Foo.../2`") ==
               ~s|<a href="AutolinkTest.Foo.html#../2"><code class="inline">AutolinkTest.Foo.../2</code></a>|

      assert autolink_doc("`AutolinkTest.Bad.bar/1`") ==
               ~s|<code class="inline">AutolinkTest.Bad.bar/1</code>|
    end

    test "elixir stdlib function" do
      assert autolink_doc("`String.upcase/2`") ==
               ~s|<a href="https://hexdocs.pm/elixir/String.html#upcase/2"><code class="inline">String.upcase/2</code></a>|
    end

    test "elixir function with default argument" do
      assert autolink_doc("`Enum.join/1`") ==
               ~s|<a href="https://hexdocs.pm/elixir/Enum.html#join/1"><code class="inline">Enum.join/1</code></a>|
    end

    test "erlang stdlib function" do
      assert autolink_doc("`:lists.all/2`") ==
               ~s|<a href="https://www.erlang.org/doc/man/lists.html#all-2"><code class="inline">:lists.all/2</code></a>|
    end

    test "local function" do
      ExDoc.Refs.insert([
        {{:module, AutolinkTest.Foo}, :public},
        {{:function, AutolinkTest.Foo, :foo, 1}, :public},
        {{:function, AutolinkTest.Foo, :., 2}, :public},
        {{:function, AutolinkTest.Foo, :.., 2}, :public}
      ])

      assert autolink_doc("`foo/1`", current_module: AutolinkTest.Foo) ==
               ~s|<a href="#foo/1"><code class="inline">foo/1</code></a>|

      assert autolink_doc("`./2`", current_module: AutolinkTest.Foo) ==
               ~s|<a href="#./2"><code class="inline">./2</code></a>|

      assert autolink_doc("`../2`", current_module: AutolinkTest.Foo) ==
               ~s|<a href="#../2"><code class="inline">../2</code></a>|

      assert autolink_doc("`bar/1`", current_module: AutolinkTest.Foo) ==
               ~s|<code class="inline">bar/1</code>|
    end

    test "auto-imported function" do
      assert autolink_doc("`+/2`") ==
               ~s|<a href="https://hexdocs.pm/elixir/Kernel.html#+/2"><code class="inline">+/2</code></a>|

      assert autolink_doc("`&/1`") ==
               ~s|<a href="https://hexdocs.pm/elixir/Kernel.SpecialForms.html#&/1"><code class="inline">&amp;/1</code></a>|

      assert autolink_doc("`for/1`") ==
               ~s|<a href="https://hexdocs.pm/elixir/Kernel.SpecialForms.html#for/1"><code class="inline">for/1</code></a>|

      assert autolink_doc("`for/1`", apps: [:elixir]) ==
               ~s|<a href="Kernel.SpecialForms.html#for/1"><code class="inline">for/1</code></a>|
    end

    @tag skip: not Version.match?(System.version(), "~> 1.13")
    test "stepped range" do
      assert autolink_doc("`..///3`") ==
               ~s|<a href="https://hexdocs.pm/elixir/Kernel.html#..///3"><code class="inline">..///3</code></a>|
    end

    test "elixir callback" do
      assert autolink_doc("`c:GenServer.handle_call/3`") ==
               ~s|<a href="https://hexdocs.pm/elixir/GenServer.html#c:handle_call/3"><code class="inline">GenServer.handle_call/3</code></a>|
    end

    test "erlang callback" do
      assert autolink_doc("`c::gen_server.handle_call/3`") ==
               ~s|<a href="https://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3"><code class="inline">:gen_server.handle_call/3</code></a>|
    end

    test "elixir type" do
      assert autolink_doc("`t:Calendar.date/0`") ==
               ~s|<a href="https://hexdocs.pm/elixir/Calendar.html#t:date/0"><code class="inline">Calendar.date/0</code></a>|
    end

    test "elixir basic & built-in types" do
      assert autolink_doc("`t:atom/0`") ==
               ~s|<a href="https://hexdocs.pm/elixir/typespecs.html#basic-types"><code class="inline">atom/0</code></a>|

      assert autolink_doc("`t:keyword/0`") ==
               ~s|<a href="https://hexdocs.pm/elixir/typespecs.html#built-in-types"><code class="inline">keyword/0</code></a>|

      assert autolink_doc("`t:keyword/0`", apps: [:elixir]) ==
               ~s|<a href="typespecs.html#built-in-types"><code class="inline">keyword/0</code></a>|
    end

    test "erlang type" do
      assert autolink_doc("`t::array.array/0`") ==
               ~s|<a href="https://www.erlang.org/doc/man/array.html#type-array"><code class="inline">:array.array/0</code></a>|
    end

    test "special forms" do
      assert autolink_doc("`__block__/1`", current_module: Kernel.SpecialForms) ==
               ~s|<a href="#__block__/1"><code class="inline">__block__/1</code></a>|

      assert autolink_doc("`__aliases__/1`", current_module: Kernel.SpecialForms) ==
               ~s|<a href="#__aliases__/1"><code class="inline">__aliases__/1</code></a>|
    end

    test "escaping" do
      assert autolink_doc("`Kernel.SpecialForms.%{}/1`") ==
               ~s|<a href="https://hexdocs.pm/elixir/Kernel.SpecialForms.html#%25%7B%7D/1"><code class="inline">Kernel.SpecialForms.%{}/1</code></a>|

      assert autolink_doc("`Kernel.SpecialForms.%/2`") ==
               ~s|<a href="https://hexdocs.pm/elixir/Kernel.SpecialForms.html#%25/2"><code class="inline">Kernel.SpecialForms.%/2</code></a>|

      assert autolink_doc("`Kernel.SpecialForms.{}/1`") ==
               ~s|<a href="https://hexdocs.pm/elixir/Kernel.SpecialForms.html#%7B%7D/1"><code class="inline">Kernel.SpecialForms.{}/1</code></a>|

      assert autolink_doc("`Kernel.SpecialForms.<<>>/1`") ==
               ~s|<a href="https://hexdocs.pm/elixir/Kernel.SpecialForms.html#%3C%3C%3E%3E/1"><code class="inline">Kernel.SpecialForms.&lt;&lt;&gt;&gt;/1</code></a>|
    end

    test "custom link" do
      assert autolink_doc("[custom text](`String`)") ==
               ~s|<a href="https://hexdocs.pm/elixir/String.html">custom text</a>|

      assert autolink_doc("[custom text](`String.at/2`)") ==
               ~s|<a href="https://hexdocs.pm/elixir/String.html#at/2">custom text</a>|

      assert autolink_doc("[custom text](`:lists`)") ==
               ~s|<a href="https://www.erlang.org/doc/man/lists.html">custom text</a>|

      assert autolink_doc("[custom text](`:lists.all/2`)") ==
               ~s|<a href="https://www.erlang.org/doc/man/lists.html#all-2">custom text</a>|
    end

    test "mix task" do
      assert autolink_doc("`mix compile.elixir`") ==
               ~s|<a href="https://hexdocs.pm/mix/Mix.Tasks.Compile.Elixir.html"><code class="inline">mix compile.elixir</code></a>|

      assert autolink_doc("`mix help compile.elixir`") ==
               ~s|<a href="https://hexdocs.pm/mix/Mix.Tasks.Compile.Elixir.html"><code class="inline">mix help compile.elixir</code></a>|

      assert autolink_doc("`mix help help`") ==
               ~s|<a href="https://hexdocs.pm/mix/Mix.Tasks.Help.html"><code class="inline">mix help help</code></a>|

      assert autolink_doc("`mix compile.elixir`", apps: [:mix]) ==
               ~s|<a href="Mix.Tasks.Compile.Elixir.html"><code class="inline">mix compile.elixir</code></a>|

      assert autolink_doc("`mix compile.elixir --verbose`") ==
               ~s|<code class="inline">mix compile.elixir --verbose</code>|

      assert autolink_doc("`mix unknown.task`") ==
               ~s|<code class="inline">mix unknown.task</code>|
    end

    test "3rd party links" do
      assert autolink_doc("`EarmarkParser.as_ast/2`") ==
               ~s|<a href="https://hexdocs.pm/earmark_parser/EarmarkParser.html#as_ast/2"><code class="inline">EarmarkParser.as_ast/2</code></a>|

      assert autolink_doc("`EarmarkParser.as_ast/2`",
               deps: [earmark_parser: "https://example.com/"]
             ) ==
               ~s|<a href="https://example.com/EarmarkParser.html#as_ast/2"><code class="inline">EarmarkParser.as_ast/2</code></a>|

      assert autolink_doc("`EarmarkParser.as_ast/2`",
               deps: [earmark_parser: "https://example.com"]
             ) ==
               ~s|<a href="https://example.com/EarmarkParser.html#as_ast/2"><code class="inline">EarmarkParser.as_ast/2</code></a>|

      # extensions are ignored for external links
      assert autolink_doc("`EarmarkParser.as_ast/2`", ext: ".xhtml") ==
               ~s|<a href="https://hexdocs.pm/earmark_parser/EarmarkParser.html#as_ast/2"><code class="inline">EarmarkParser.as_ast/2</code></a>|
    end

    test "extras" do
      opts = [extras: %{"Foo Bar.md" => "foo-bar", "Bar Baz.livemd" => "bar-baz"}]

      assert autolink_doc("[Foo](Foo Bar.md)", opts) == ~s|<a href="foo-bar.html">Foo</a>|

      assert autolink_doc("[Bar](Bar Baz.livemd)", opts) == ~s|<a href="bar-baz.html">Bar</a>|

      assert autolink_doc("[Foo](Foo Bar.md)", [ext: ".xhtml"] ++ opts) ==
               ~s|<a href="foo-bar.xhtml">Foo</a>|

      assert autolink_doc("[Foo](Foo Bar.md#baz)", opts) == ~s|<a href="foo-bar.html#baz">Foo</a>|

      assert autolink_doc("[Foo](../guide/Foo Bar.md)", opts) ==
               ~s|<a href="foo-bar.html">Foo</a>|

      assert autolink_doc("[Foo](http://example.com/foo.md)", opts) ==
               ~s|<a href="http://example.com/foo.md">Foo</a>|

      assert autolink_doc("[Foo](#baz)", opts) == ~s|<a href="#baz">Foo</a>|
    end

    test "special case links" do
      assert autolink_doc("`//2`") ==
               ~s|<a href="https://hexdocs.pm/elixir/Kernel.html#//2"><code class="inline">//2</code></a>|

      assert autolink_doc("[division](`//2`)") ==
               ~s|<a href="https://hexdocs.pm/elixir/Kernel.html#//2">division</a>|

      assert autolink_doc("`Kernel.//2`") ==
               ~s|<a href="https://hexdocs.pm/elixir/Kernel.html#//2"><code class="inline">Kernel.//2</code></a>|

      assert autolink_doc("[division](`Kernel.//2`)") ==
               ~s|<a href="https://hexdocs.pm/elixir/Kernel.html#//2">division</a>|
    end

    test "other link" do
      assert autolink_doc("[`String`](foo.html)") ==
               ~s|<a href="foo.html"><code class="inline">String</code></a>|

      assert autolink_doc("[custom text](foo.html)") == ~s|<a href="foo.html">custom text</a>|
    end

    test "other" do
      assert autolink_doc("`String.upcase() / 2`") ==
               ~s|<code class="inline">String.upcase() / 2</code>|

      assert autolink_doc("`:\"atom\"`") ==
               ~s|<code class="inline">:&quot;atom&quot;</code>|

      assert autolink_doc("`1 + 2`") ==
               ~s|<code class="inline">1 + 2</code>|

      assert autolink_doc("hello") ==
               "hello"
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

    test "skips autolinking if requested" do
      ExDoc.Refs.insert([
        {{:module, AutolinkTest.Hidden}, :hidden},
        {{:function, AutolinkTest.Hidden, :foo, 1}, :hidden}
      ])

      options = [
        skip_code_autolink_to: [
          "AutolinkTest.Hidden",
          "AutolinkTest.Hidden.foo/1"
        ]
      ]

      assert autolink_doc("`AutolinkTest.Hidden`", options) ==
               ~s|<code class="inline">AutolinkTest.Hidden</code>|

      assert autolink_doc("`AutolinkTest.Hidden.foo/1`", options) ==
               ~s|<code class="inline">AutolinkTest.Hidden.foo/1</code>|
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

  defmodule Elixir.InMemory do
    @callback hello() :: :world
    def hello(), do: :world
  end

  test "in-memory module" do
    assert autolink_doc("`InMemory.hello/0`") ==
             ~s|<a href="InMemory.html#hello/0"><code class="inline">InMemory.hello/0</code></a>|

    assert autolink_doc("`c:InMemory.hello/0`") ==
             ~s|<a href="InMemory.html#c:hello/0"><code class="inline">InMemory.hello/0</code></a>|

    assert warn(fn ->
             assert autolink_doc("`InMemory.unknown/0`", warnings: :send) ==
                      ~s|<code class="inline">InMemory.unknown/0</code>|
           end)

    assert warn(fn ->
             assert autolink_doc("`c:InMemory.unknown/0`", warnings: :send) ==
                      ~s|<code class="inline">c:InMemory.unknown/0</code>|
           end)

    # Types are not checked for in memory
    assert autolink_doc("`t:InMemory.unknown/0`") ==
             ~s|<code class="inline">t:InMemory.unknown/0</code>|
  end

  test "warning if typespec references filtered module" do
    ExDoc.Refs.insert([
      {{:module, AutolinkTest.Keep}, :public},
      {{:function, AutolinkTest.Filtered}, :public},
      {{:type, AutolinkTest.Filtered, :type, 0}, :public}
    ])

    # TODO: testing
  end

  test "warnings" do
    ExDoc.Refs.insert([
      {{:module, AutolinkTest.Foo}, :public},
      {{:function, AutolinkTest.Foo, :bar, 1}, :hidden},
      {{:type, AutolinkTest.Foo, :bad, 0}, :hidden}
    ])

    opts = [
      warnings: :send
    ]

    assert warn(fn ->
             assert autolink_doc("`AutolinkTest.Foo.bar/1`", opts) ==
                      ~s|<code class="inline">AutolinkTest.Foo.bar/1</code>|
           end) =~
             ~s|documentation references function "AutolinkTest.Foo.bar/1" but it is hidden|

    assert warn(fn ->
             assert autolink_doc("`t:AutolinkTest.Foo.bad/0`", opts) ==
                      ~s|<code class="inline">t:AutolinkTest.Foo.bad/0</code>|
           end) =~
             ~s|documentation references type "t:AutolinkTest.Foo.bad/0" but it is hidden or private|

    assert warn(fn ->
             assert autolink_doc("`t:Elixir.AutolinkTest.Foo.bad/0`", opts) ==
                      ~s|<code class="inline">t:Elixir.AutolinkTest.Foo.bad/0</code>|
           end) =~
             ~s|documentation references type "t:Elixir.AutolinkTest.Foo.bad/0" but it is hidden or private|

    assert warn(fn ->
             assert autolink_doc("`t:AutolinkTest.Foo.bad/0`", opts) ==
                      ~s|<code class="inline">t:AutolinkTest.Foo.bad/0</code>|
           end) =~
             ~s|documentation references type "t:AutolinkTest.Foo.bad/0" but it is hidden or private|

    assert warn(fn ->
             assert autolink_doc("`Code.Typespec`", opts) ==
                      ~s|<code class="inline">Code.Typespec</code>|
           end) =~
             ~s|documentation references module "Code.Typespec" but it is hidden|

    assert warn(fn ->
             assert autolink_doc("`String.upcase/9`", opts) ==
                      ~s|<code class="inline">String.upcase/9</code>|
           end)

    assert warn(fn ->
             assert autolink_doc("`c:GenServer.handle_call/9`", opts) ==
                      ~s|<code class="inline">c:GenServer.handle_call/9</code>|
           end)

    assert warn(fn ->
             assert autolink_doc("`t:Calendar.date/9`", opts) ==
                      ~s|<code class="inline">t:Calendar.date/9</code>|
           end)

    assert warn(fn ->
             assert autolink_doc("[text](`foo/0`)", opts) == "text"
           end) =~ ~s|documentation references function "foo/0" but it is undefined or private|

    assert warn(fn ->
             assert autolink_doc("[text](`fakefunction`)", opts) == "text"
           end) =~ ~s|documentation references "fakefunction" but it is invalid|

    assert warn(fn ->
             assert autolink_doc("[text](`some.function`)", opts) == "text"
           end) =~ ~s|documentation references "some.function" but it is invalid|

    assert warn(fn ->
             assert autolink_doc("[text](`Enum.map()`)", opts) == "text"
           end) =~ ~s|documentation references "Enum.map()" but it is invalid|

    assert warn(fn ->
             assert autolink_doc("[text](`t:supervisor.child_spec/0`)", opts) == "text"
           end) =~ ~s|documentation references "t:supervisor.child_spec/0" but it is invalid|

    assert warn(fn ->
             autolink_spec(quote(do: t() :: String.bad()), opts)
           end) =~ ~s|documentation references type "String.bad()"|

    assert warn(fn ->
             autolink_spec(
               quote do
                 t() :: %{
                   name: String.bad()
                 }
               end,
               opts
             )
           end) =~ ~s|documentation references type "String.bad()"|

    assert warn(fn ->
             assert autolink_doc("[Foo](Foo Bar.md)", opts ++ [extras: %{}]) ==
                      ~s|<a href="Foo Bar.md">Foo</a>|
           end) =~ ~s|documentation references file "Foo Bar.md" but it does not exist|

    assert warn(fn ->
             assert autolink_doc("[Bar A](`Bar.A`)", opts) == "Bar A"
           end) =~ ~s|module "Bar.A" but it is undefined|

    assert autolink_doc("`Bar.A`", opts) == ~s|<code class="inline">Bar.A</code>|

    assert warn(fn ->
             assert autolink_doc("[custom text](`Elixir.Unknown`)", opts) == "custom text"
           end) =~ ~s|documentation references module "Elixir.Unknown" but it is undefined|

    assert warn(fn ->
             assert autolink_doc("[custom `text`](`Elixir.Unknown`)", opts) ==
                      ~s|custom <code class="inline">text</code>|
           end)

    assert warn(fn ->
             assert autolink_doc("[It is Unknown](`Unknown`)", opts) == "It is Unknown"
           end) =~ ~s|documentation references module "Unknown" but it is undefined|

    assert warn(fn ->
             assert autolink_doc("[Foo task](`mix foo`)", opts) == ~s|Foo task|
           end) =~ ~s|documentation references "mix foo" but it is undefined|

    assert autolink_doc("`mix foo`", opts) ==
             ~s|<code class="inline">mix foo</code>|

    assert warn(fn ->
             assert autolink_doc("[bad](`String.upcase/9`)", opts ++ [extras: %{}]) == "bad"
           end) =~
             ~s|documentation references function "String.upcase/9" but it is undefined or private|

    assert autolink_doc("`Unknown`") == ~s|<code class="inline">Unknown</code>|

    assert autolink_doc("[Blank](about:blank)") == ~s|<a href="about:blank">Blank</a>|

    assert autolink_doc("`FOR UPDATE OF ? SKIP LOCKED`") ==
             ~s|<code class="inline">FOR UPDATE OF ? SKIP LOCKED</code>|

    opts = [
      warnings: :send,
      skip_undefined_reference_warnings_on: ["MyModule"],
      module_id: "MyModule"
    ]

    assert autolink_doc("`String.upcase/9`", opts) ==
             ~s|<code class="inline">String.upcase/9</code>|
  end

  ## Helpers

  @default_options [
    apps: [:myapp],
    current_module: MyModule,
    module_id: "MyModule",
    file: "nofile",
    language: ExDoc.Language.Elixir
  ]

  defp autolink_doc(text, options \\ []) when is_binary(text) do
    text
    |> ExDoc.Markdown.to_ast([])
    |> ExDoc.Language.Elixir.autolink_doc(Keyword.merge(@default_options, options))
    |> then(fn [{:p, _, content, _}] -> content end)
    |> ExDoc.DocAST.to_string()
  end

  defp warn(fun) when is_function(fun, 0) do
    fun.()
    assert_received {:warn, message, _metadata}
    message
  end

  defp autolink_spec(ast, options \\ []) do
    ExDoc.Language.Elixir.autolink_spec(ast, Keyword.merge(@default_options, options))
  end
end
