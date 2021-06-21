defmodule ExDoc.Language.ElixirTest do
  use ExUnit.Case, async: true

  describe "autolink_doc/2" do
    test "module" do
      assert doc("`String`") ==
               ~s{<p><a href="https://hexdocs.pm/elixir/String.html"><code class="inline">String</code></a></p>}

      assert doc("`Bad`") ==
               ~s{<p><code class="inline">Bad</code></p>}

      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert doc("`String.Unicode`") ==
                        ~s{<p><code class="inline">String.Unicode</code></p>}
             end) =~ "documentation references \"String.Unicode\" but it is hidden"
    end

    test "local function" do
      assert doc("`upcase/2`", module: String) ==
               ~s{<p><a href="#upcase/2"><code class="inline">upcase/2</code></a></p>}

      assert doc("`bad/0`", module: String) ==
               ~s{<p><code class="inline">bad/0</code></p>}
    end

    test "auto-imported function" do
      assert doc("`elem/2`") ==
               ~s{<p><a href="https://hexdocs.pm/elixir/Kernel.html#elem/2"><code class="inline">elem/2</code></a></p>}
    end

    test "remote function" do
      assert doc("`String.upcase/2`") ==
               ~s{<p><a href="https://hexdocs.pm/elixir/String.html#upcase/2"><code class="inline">String.upcase/2</code></a></p>}

      assert doc("`:gen_server.call/3`") ==
               ~s{<p><a href="https://erlang.org/doc/man/gen_server.html#call-3"><code class="inline">:gen_server.call/3</code></a></p>}

      assert doc("`Bad.bad/0`") ==
               ~s{<p><code class="inline">Bad.bad/0</code></p>}

      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert doc("`String.upcase/9`") ==
                        ~s{<p><code class="inline">String.upcase/9</code></p>}
             end) =~ "documentation references \"String.upcase/9\" but it is undefined"
    end

    test "remote function from OTP that is not loaded by default" do
      assert :xmerl not in Enum.map(Application.loaded_applications(), &elem(&1, 0))

      assert doc("`:xmerl_scan.string/1`") ==
               ~s{<p><a href="https://erlang.org/doc/man/xmerl_scan.html#string-1"><code class="inline">:xmerl_scan.string/1</code></a></p>}
    end

    @tag :tmp_dir
    test "local type", c do
      assert doc("`t:t/0`", module: String) ==
               ~s{<p><a href="#t:t/0"><code class="inline">t/0</code></a></p>}

      assert doc("`t:bad/0`", module: String) ==
               ~s{<p><code class="inline">bad/0</code></p>}

      TestHelper.elixirc(c, """
      defmodule ExDoc.Language.ElixirTest.Foo do
        @type t() :: typep()

        @typep typep() :: term()
      end
      """)

      assert doc("`t:typep/0`", module: ExDoc.Language.ElixirTest.Foo) ==
               ~s{<p><code class="inline">typep/0</code></p>}
    end

    test "basic and built-in type" do
      assert doc("`t:atom/0`") ==
               ~s{<p><a href="https://hexdocs.pm/elixir/typespecs.html#basic-types"><code class="inline">atom/0</code></a></p>}

      assert doc("`t:keyword/0`") ==
               ~s{<p><a href="https://hexdocs.pm/elixir/typespecs.html#built-in-types"><code class="inline">keyword/0</code></a></p>}
    end

    test "remote type" do
      assert doc("`t:String.t/0`") ==
               ~s{<p><a href="https://hexdocs.pm/elixir/String.html#t:t/0"><code class="inline">String.t/0</code></a></p>}

      assert doc("`t::array.array/0`") ==
               ~s{<p><a href="https://erlang.org/doc/man/array.html#type-array"><code class="inline">:array.array/0</code></a></p>}

      assert doc("`t:Bad.bad/0`") ==
               ~s{<p><code class="inline">Bad.bad/0</code></p>}

      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert doc("`t:String.bad/0`") ==
                        ~s{<p><code class="inline">String.bad/0</code></p>}
             end) =~ "documentation references \"t:String.bad/0\" but it is undefined"
    end

    test "local callback" do
      assert doc("`c:handle_call/3`", module: GenServer) ==
               ~s{<p><a href="#c:handle_call/3"><code class="inline">handle_call/3</code></a></p>}

      assert doc("`c:bad/0`", module: GenServer) ==
               ~s{<p><code class="inline">bad/0</code></p>}
    end

    test "remote callback" do
      assert doc("`c:GenServer.handle_call/3`") ==
               ~s{<p><a href="https://hexdocs.pm/elixir/GenServer.html#c:handle_call/3"><code class="inline">GenServer.handle_call/3</code></a></p>}

      assert doc("`c::gen_server.handle_call/3`") ==
               ~s{<p><a href="https://erlang.org/doc/man/gen_server.html#Module:handle_call-3"><code class="inline">:gen_server.handle_call/3</code></a></p>}

      assert doc("`c:Bad.bad/0`") ==
               ~s{<p><code class="inline">Bad.bad/0</code></p>}

      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert doc("`c:GenServer.bad/0`") ==
                        ~s{<p><code class="inline">GenServer.bad/0</code></p>}
             end) =~ "documentation references \"c:GenServer.bad/0\" but it is undefined"
    end

    test "mix task" do
      assert doc("`mix test`") ==
               ~s{<p><a href="https://hexdocs.pm/mix/Mix.Tasks.Test.html"><code class="inline">mix test</code></a></p>}
    end

    test "custom link" do
      assert doc("[module](`String`)") ==
               ~s{<p><a href="https://hexdocs.pm/elixir/String.html">module</a></p>}

      assert doc("[`String` module](`String`)") ==
               ~s{<p><a href="https://hexdocs.pm/elixir/String.html"><code class="inline">String</code> module</a></p>}

      assert doc("[`in` operator](`in/2`)", ext: ".xhtml") ==
               ~s{<p><a href="https://hexdocs.pm/elixir/Kernel.html#in/2"><code class="inline">in</code> operator</a></p>}

      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert doc("[bad](`bad/0`)", module: String) ==
                        ~s{<p>bad</p>}
             end) =~ "bad/0"

      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert doc("[bad](`mix bad`)") ==
                        ~s{<p>bad</p>}
             end) =~ "documentation references \"mix bad\" but it is undefined"
    end

    test "special names" do
      assert doc("`//2`") ==
               ~s{<p><a href="https://hexdocs.pm/elixir/Kernel.html#//2"><code class="inline">//2</code></a></p>}

      assert doc("`Kernel.//2`") ==
               ~s{<p><a href="https://hexdocs.pm/elixir/Kernel.html#//2"><code class="inline">Kernel.//2</code></a></p>}
    end

    test "3rd party links" do
      assert doc("`String`") ==
               ~s{<p><a href="https://hexdocs.pm/elixir/String.html"><code class="inline">String</code></a></p>}

      assert doc("`EarmarkParser.as_ast/2`") ==
               ~s{<p><a href="https://hexdocs.pm/earmark_parser/EarmarkParser.html#as_ast/2"><code class="inline">EarmarkParser.as_ast/2</code></a></p>}

      assert doc("`EarmarkParser.as_ast/2`", deps: [earmark_parser: "https://example.com/"]) ==
               ~s{<p><a href="https://example.com/EarmarkParser.html#as_ast/2"><code class="inline">EarmarkParser.as_ast/2</code></a></p>}

      # extensions are ignored as they are external links
      assert doc("`EarmarkParser.as_ast/2`", ext: ".xhtml") ==
               ~s{<p><a href="https://hexdocs.pm/earmark_parser/EarmarkParser.html#as_ast/2"><code class="inline">EarmarkParser.as_ast/2</code></a></p>}
    end

    test "extras" do
      opts = [extras: ["Foo Bar.md"], file: "foo.ex", line: 1]

      assert doc("[Foo](Foo Bar.md)", opts) ==
               ~s{<p><a href="foo-bar.html">Foo</a></p>}

      warning =
        ExUnit.CaptureIO.capture_io(:stderr, fn ->
          assert doc("[Foo](Bad.md)", opts) ==
                   ~s{<p><a href="Bad.md">Foo</a></p>}
        end)

      assert warning =~ "documentation references file \"Bad.md\" but it does not exists"
      assert warning =~ "foo.ex:1"

      assert doc("[Foo](Foo Bar.md)", [ext: ".xhtml"] ++ opts) ==
               ~s{<p><a href="foo-bar.xhtml">Foo</a></p>}

      assert doc("[Foo](Foo Bar.md#baz)", opts) ==
               ~s{<p><a href="foo-bar.html#baz">Foo</a></p>}

      assert doc("[Foo](../guide/Foo Bar.md)", opts) ==
               ~s{<p><a href="foo-bar.html">Foo</a></p>}

      assert doc("[Foo](http://example.com/foo.md)", opts) ==
               ~s{<p><a href="http://example.com/foo.md">Foo</a></p>}

      assert doc("[Foo](#bar)", opts) ==
               ~s{<p><a href="#bar">Foo</a></p>}
    end
  end

  defp doc(doc, opts \\ []) do
    config = struct!(ExDoc.Autolink, opts)

    doc
    |> ExDoc.DocAST.parse!("text/markdown")
    |> ExDoc.Language.Elixir.autolink_doc(config)
    |> ExDoc.DocAST.to_string()
  end

  describe "autolink_spec/2" do
    test "operators" do
      ExDoc.Refs.insert([
        {{:module, MyModule}, :public},
        {{:type, MyModule, :foo, 0}, :public}
      ])

      assert spec(quote(do: +foo() :: foo())) ==
               ~s[+<a href="#t:foo/0">foo</a>() :: <a href="#t:foo/0">foo</a>()]

      assert spec(quote(do: foo() + foo() :: foo())) ==
               ~s[<a href=\"#t:foo/0\">foo</a>() + <a href=\"#t:foo/0\">foo</a>() :: <a href=\"#t:foo/0\">foo</a>()]

      assert spec(quote(do: -0 :: 0)) == ~s[-0 :: 0]
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

      assert spec(quote(do: unquote(:"/=")() :: :ok)) ==
               ~s[/=() :: :ok]

      assert spec(quote(do: t() :: foo(1))) ==
               ~s[t() :: <a href="#t:foo/1">foo</a>(1)]

      assert spec(quote(do: t() :: bar(foo(1)))) ==
               ~s[t() :: <a href=\"#t:bar/1\">bar</a>(<a href=\"#t:foo/1\">foo</a>(1))]

      assert spec(quote(do: (t() :: bar(foo(1)) when bat: foo(1)))) ==
               ~s[t() :: <a href=\"#t:bar/1\">bar</a>(<a href="#t:foo/1">foo</a>(1)) when bat: <a href=\"#t:foo/1\">foo</a>(1)]

      assert spec(quote(do: t() :: bar(baz(1)))) ==
               ~s[t() :: <a href=\"#t:bar/1\">bar</a>(<a href=\"#t:baz/1\">baz</a>(1))]

      assert spec(quote(do: t() :: foo(bar(), bar()))) ==
               ~s[t() :: <a href="#t:foo/2">foo</a>(<a href=\"#t:bar/0\">bar</a>(), <a href=\"#t:bar/0\">bar</a>())]

      assert spec(quote(do: t() :: foo!(bar()))) ==
               ~s[t() :: <a href="#t:foo!/1">foo!</a>(<a href=\"#t:bar/0\">bar</a>())]

      # TODO: probably update this according to the previous test's changes
      assert spec(quote(do: t() :: foo?(bar()))) ==
               ~s[t() :: <a href="#t:foo?/1">foo?</a>(<a href=\"#t:bar/0\">bar</a>())]

      assert spec(
               quote do
                 t() :: %{
                   required(bar()) => bar(),
                   optional(bar()) => bar()
                 }
               end
             ) ==
               "t() :: %{required(<a href=\"#t:bar/0\">bar</a>()) =&gt; <a href=\"#t:bar/0\">bar</a>(), optional(<a href=\"#t:bar/0\">bar</a>()) =&gt; <a href=\"#t:bar/0\">bar</a>()}"
    end

    test "remotes" do
      assert spec(quote(do: t() :: String.t())) ==
               ~s[t() :: <a href="https://hexdocs.pm/elixir/String.html#t:t/0">String.t</a>()]

      assert spec(quote(do: t() :: String.t()), module: String) ==
               ~s[t() :: <a href="#t:t/0">String.t</a>()]

      assert spec(quote(do: t() :: String.t()), apps: [:elixir]) ==
               ~s[t() :: <a href="String.html#t:t/0">String.t</a>()]
    end

    test "autolinks same type and function name" do
      ExDoc.Refs.insert([
        {{:module, MyModule}, :public},
        {{:type, MyModule, :foo, 0}, :public},
        {{:type, MyModule, :foo, 1}, :public}
      ])

      assert spec(quote(do: foo() :: foo())) ==
               ~s[foo() :: <a href="#t:foo/0">foo</a>()]

      assert spec(quote(do: foo(1) :: foo(1))) ==
               ~s[foo(1) :: <a href="#t:foo/1">foo</a>(1)]

      assert spec(quote(do: (foo(1) :: foo(1) when bat: foo(1)))) ==
               ~s[foo(1) :: <a href=\"#t:foo/1\">foo</a>(1) when bat: <a href=\"#t:foo/1\">foo</a>(1)]

      assert spec(quote(do: bar(foo(1)) :: foo(1))) ==
               ~s[bar(<a href=\"#t:foo/1\">foo</a>(1)) :: <a href=\"#t:foo/1\">foo</a>(1)]

      assert spec(quote(do: (bar(foo(1)) :: foo(1) when bat: foo(1)))) ==
               ~s[bar(<a href=\"#t:foo/1\">foo</a>(1)) :: <a href=\"#t:foo/1\">foo</a>(1) when bat: <a href=\"#t:foo/1\">foo</a>(1)]

      assert spec(quote(do: bar(foo :: foo(1)) :: foo(1))) ==
               ~s[bar(foo :: <a href=\"#t:foo/1\">foo</a>(1)) :: <a href=\"#t:foo/1\">foo</a>(1)]
    end

    test "Elixir basic types" do
      assert spec(quote(do: t() :: atom())) ==
               ~s[t() :: <a href=\"https://hexdocs.pm/elixir/typespecs.html#basic-types\">atom</a>()]
    end

    test "Elixir built-in types" do
      assert spec(quote(do: t() :: keyword())) ==
               ~s[t() :: <a href=\"https://hexdocs.pm/elixir/typespecs.html#built-in-types\">keyword</a>()]
    end

    test "Erlang stdlib types" do
      assert spec(quote(do: t() :: :sets.set())) ==
               ~s[t() :: <a href=\"https://erlang.org/doc/man/sets.html#type-set\">:sets.set</a>()]
    end

    test "escape special HTML characters" do
      assert spec(quote(do: term() < term() :: boolean())) ==
               ~s[<a href="https://hexdocs.pm/elixir/typespecs.html#built-in-types">term</a>() &lt; <a href="https://hexdocs.pm/elixir/typespecs.html#built-in-types">term</a>() :: <a href="https://hexdocs.pm/elixir/typespecs.html#built-in-types">boolean</a>()]
    end
  end

  @default_options [
    module: MyModule
  ]

  defp spec(ast, options \\ []) do
    config = struct!(ExDoc.Autolink, Keyword.merge(@default_options, options))
    ExDoc.Language.Elixir.autolink_spec(ast, config)
  end
end
