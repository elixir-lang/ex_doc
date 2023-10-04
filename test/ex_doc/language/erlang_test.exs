defmodule ExDoc.Language.ErlangTest do
  use ExUnit.Case, async: true
  import TestHelper

  @moduletag :otp_has_docs
  @moduletag :tmp_dir

  describe "autolink_doc/2" do
    test "module", c do
      assert autolink_doc("{@link erlang_bar}", c) ==
               ~s|<a href="erlang_bar.html"><code>erlang_bar</code></a>|
    end

    test "current module", c do
      assert autolink_doc("{@link erlang_foo}", [current_module: :erlang_foo], c) ==
               ~s|<a href="erlang_foo.html#content"><code>erlang_foo</code></a>|
    end

    test "OTP module", c do
      assert autolink_doc("{@link array}", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/array.html"><code>array</code></a>|
    end

    test "OTP module when generating OTP docs", c do
      assert autolink_doc("{@link array}", [deps: [stdlib: "https://example.com/stdlib"]], c) ==
               ~s|<a href="https://example.com/stdlib/array.html"><code>array</code></a>|
    end

    test "app module", c do
      assert autolink_doc("{@link //stdlib/array}", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/array.html"><code>array</code></a>|
    end

    test "external module", c do
      assert autolink_doc("{@link 'Elixir.EarmarkParser'}", c) ==
               ~s|<a href="https://hexdocs.pm/earmark_parser/EarmarkParser.html"><code>'Elixir.EarmarkParser'</code></a>|
    end

    test "external module - extension is ignored", c do
      assert autolink_doc("{@link 'Elixir.EarmarkParser'}", [ext: ".xhtml"], c) ==
               ~s|<a href="https://hexdocs.pm/earmark_parser/EarmarkParser.html"><code>'Elixir.EarmarkParser'</code></a>|
    end

    test "module with anchor" do
      ast =
        {:a, [href: "array#anchor", rel: "https://erlang.org/doc/link/seeerl"],
         [{:code, [], ["array"], %{}}], %{}}

      assert do_autolink_doc(ast) ==
               ~s|<a href="https://www.erlang.org/doc/man/array.html#anchor"><code>array</code></a>|

      ast =
        {:a, [href: "stdlib:array#anchor", rel: "https://erlang.org/doc/link/seeerl"],
         [{:code, [], ["array"], %{}}], %{}}

      assert do_autolink_doc(ast) ==
               ~s|<a href="https://www.erlang.org/doc/man/array.html#anchor"><code>array</code></a>|
    end

    test "custom text", c do
      assert autolink_doc("{@link array. The <code>array</code> module}", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/array.html">The <code>array</code> module</a>|
    end

    test "local function", c do
      assert autolink_doc("{@link foo/0}", [current_module: :erlang_foo], c) ==
               ~s|<a href="#foo/0"><code>foo/0</code></a>|
    end

    test "remote function", c do
      assert autolink_doc("{@link erlang_bar:bar/0}", c) ==
               ~s|<a href="erlang_bar.html#bar/0"><code>erlang_bar:bar/0</code></a>|
    end

    test "OTP function", c do
      assert autolink_doc("{@link array:new/0}", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/array.html#new-0"><code>array:new/0</code></a>|
    end

    test "OTP function when generating OTP docs", c do
      assert autolink_doc("{@link array:new/0}", [apps: [:stdlib]], c) ==
               ~s|<a href="array.html#new/0"><code>array:new/0</code></a>|
    end

    test "OTP function when generating OTP docs, same module", c do
      assert autolink_doc("{@link array:new/0}", [current_module: :array, apps: [:stdlib]], c) ==
               ~s|<a href="array.html#new/0"><code>array:new/0</code></a>|
    end

    test "ERTS function", c do
      assert autolink_doc("{@link zlib:gunzip/1}", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/zlib.html#gunzip-1"><code>zlib:gunzip/1</code></a>|
    end

    test "app function", c do
      assert autolink_doc("{@link //stdlib/array:new/0}", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/array.html#new-0"><code>array:new/0</code></a>|
    end

    # TODO: test callbacks. No support in EDoc, use :docgen_xml_to_chunks.

    test "external function", c do
      assert autolink_doc("{@link 'Elixir.EarmarkParser':as_ast/2}", c) ==
               ~s|<a href="https://hexdocs.pm/earmark_parser/EarmarkParser.html#as_ast/2"><code>'Elixir.EarmarkParser':as_ast/2</code></a>|
    end

    test "local type", c do
      assert autolink_doc("{@link t()}", [current_module: :erlang_foo], c) ==
               ~s|<a href="#t:t/0"><code>t()</code></a>|
    end

    test "remote type", c do
      assert autolink_doc("{@link erlang_bar:t()}", c) ==
               ~s|<a href="erlang_bar.html#t:t/0"><code>erlang_bar:t()</code></a>|
    end

    test "OTP type", c do
      assert autolink_doc("{@link array:array()}", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/array.html#type-array"><code>array:array()</code></a>|
    end

    test "app type", c do
      assert autolink_doc("{@link //stdlib/array:array()}", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/array.html#type-array"><code>array:array()</code></a>|
    end

    test "abstract types - description", c do
      assert autolink_doc("{@type myList(X). A special kind of lists ...}", c) ==
               ~s|<code><a href=\"#type-myList\">myList</a>(X)</code>|
    end

    test "abstract types - description+dot", c do
      assert autolink_doc("{@type myList(X, Y).}", c) ==
               ~s|<code><a href=\"#type-myList\">myList</a>(X, Y)</code>|
    end

    test "abstract types - no description", c do
      assert autolink_doc("{@type myList()}", c) ==
               ~s|<code><a href=\"#type-myList\">myList()</a></code>|
    end

    test "bad module", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_doc("{@link bad}", c) == ~s|<code>bad</code>|
             end) =~ ~s|references module "bad" but it is undefined|
    end

    test "bad local function", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_doc("{@link bad/0}", c) == ~s|<code>bad/0</code>|
             end) =~ ~s|references function "bad/0" but it is undefined or private|
    end

    test "bad remote function", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_doc("{@link bad:bad/0}", c) == ~s|<code>bad:bad/0</code>|
             end) =~ ~s|references function "bad:bad/0" but it is undefined or private|
    end

    test "bad local type", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_doc("{@link bad()}", c) == ~s|<code>bad()</code>|
             end) =~ ~s|references type "bad()" but it is undefined or private|
    end

    test "bad remote type", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_doc("{@link bad:bad()}", c) == ~s|<code>bad:bad()</code>|
             end) =~ ~s|references type "bad:bad()" but it is undefined or private|
    end

    test "application", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_doc("{@link //foo}", c) == ~s|<code>//foo</code>|
             end) =~ ~r{invalid reference: foo:index}
    end
  end

  describe "autolink_doc/2 for extras" do
    test "function", c do
      assert autolink_extra("`erlang_foo:foo/0`", c) ==
               ~s|<a href="erlang_foo.html#foo/0"><code class="inline">erlang_foo:foo/0</code></a>|
    end

    test "OTP function", c do
      assert autolink_extra("`lists:reverse/1`", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/lists.html#reverse-1"><code class="inline">lists:reverse/1</code></a>|
    end

    test "type", c do
      assert autolink_extra("`t:erlang_bar:t()`", c) ==
               ~s|<a href="erlang_bar.html#t:t/0"><code class="inline">erlang_bar:t()</code></a>|
    end

    test "OTP type", c do
      assert autolink_extra("`t:array:array()`", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/array.html#type-array"><code class="inline">array:array()</code></a>|
    end

    test "module", c do
      assert autolink_extra("`erlang_foo`", c) ==
               ~s|<a href="erlang_foo.html"><code class="inline">erlang_foo</code></a>|
    end

    test "OTP module", c do
      assert autolink_extra("`rpc`", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/rpc.html"><code class="inline">rpc</code></a>|
    end

    test "bad function", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_extra("`bad:bad/0`", c) ==
                        ~s|<code class="inline">bad:bad/0</code>|
             end) =~ ~s|references function "bad:bad/0" but it is undefined or private|
    end

    test "bad type", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_extra("`t:bad:bad/0`", c) ==
                        ~s|<code class="inline">t:bad:bad/0</code>|
             end) =~ ~s|references type "bad:bad()" but it is undefined or private|
    end

    test "bad module", c do
      assert autolink_extra("`does_not_exist`", c) ==
               ~s|<code class="inline">does_not_exist</code>|
    end
  end

  describe "autolink_spec/2" do
    test "spec", c do
      assert autolink_spec("-spec foo() -> t().", c) ==
               ~s|foo() -> <a href="#t:t/0">t</a>().|
    end

    test "spec when fun is called record", c do
      assert autolink_spec("-spec record(module()) -> [[{module(), atom()}]].", c) ==
               ~s|record(module()) -> [[{module(), atom()}]].|
    end

    test "callback", c do
      assert autolink_spec("-callback foo() -> t().", c) ==
               ~s|foo() -> <a href="#t:t/0">t</a>().|
    end

    test "type", c do
      assert autolink_spec("-type foo() :: t().", c) ==
               ~s|foo() :: <a href="#t:t/0">t</a>().|
    end

    test "opaque", c do
      assert autolink_spec("-opaque foo() :: t().", c) ==
               ~s|foo()|
    end

    test "opaque with variables", c do
      assert autolink_spec("-opaque foo(X, Y) :: X | Y.", c) ==
               ~s|foo(X, Y)|
    end

    test "tuple", c do
      assert autolink_spec(~S"-spec foo() -> {ok, t()}.", c) ==
               ~s|foo() -> {ok, <a href="#t:t/0">t</a>()}.|
    end

    test "list", c do
      assert autolink_spec(~S"-spec foo() -> [t()].", c) ==
               ~s|foo() -> [<a href="#t:t/0">t</a>()].|
    end

    test "map", c do
      assert autolink_spec(~S"-spec foo() -> #{atom() := string(), float() => t()}.", c) ==
               ~S|foo() -> #{atom() := string(), float() => <a href="#t:t/0">t</a>()}.|
    end

    test "vars", c do
      assert autolink_spec(~s"-spec foo(X) -> {Y :: t(), X}.", c) ==
               ~s|foo(X) -> {Y :: <a href="#t:t/0">t</a>(), X}.|
    end

    test "union", c do
      assert autolink_spec(~s"-spec foo() -> ok | t().", c) ==
               ~s[foo() -> ok | <a href="#t:t/0">t</a>().]
    end

    test "record - empty", c do
      assert autolink_spec(~s"-spec foo() -> #x{} | t().", c) ==
               ~s[foo() -> #x{} | <a href="#t:t/0">t</a>().]
    end

    test "record - one field", c do
      assert autolink_spec(~s"-spec foo() -> #x{x :: atom()} | t().", c) ==
               ~s[foo() -> #x{x :: atom()} | <a href="#t:t/0">t</a>().]
    end

    test "record - two fields", c do
      assert autolink_spec(~s"-spec foo() -> #x{x :: atom(), y :: integer()} | t().", c) ==
               ~s[foo() -> #x{x :: atom(), y :: integer()} | <a href="#t:t/0">t</a>().]
    end

    test "bitstring", c do
      assert autolink_spec(~s"-spec foo() -> <<_:_*16>> | t().", c) ==
               ~s[foo() -> <<_:_*16>> | <a href="#t:t/0">t</a>().]
    end

    test "integer range", c do
      assert autolink_spec(~s"-spec foo() -> 1..255 | t().", c) ==
               ~s[foo() -> 1..255 | <a href="#t:t/0">t</a>().]
    end

    test "function - any", c do
      assert autolink_spec(~s"-spec foo() -> fun() | t().", c) ==
               ~s[foo() -> fun() | <a href="#t:t/0">t</a>().]
    end

    test "function - any arity", c do
      assert autolink_spec(~s"-spec foo() -> fun((...) -> t()) | erlang_bar:t().", c) ==
               ~s[foo() -> fun((...) -> <a href="#t:t/0">t</a>()) | <a href="erlang_bar.html#t:t/0">erlang_bar:t</a>().]
    end

    test "local type", c do
      assert autolink_spec(~S"-spec foo() -> t().", c) ==
               ~s|foo() -> <a href="#t:t/0">t</a>().|
    end

    test "remote type", c do
      assert autolink_spec(~S"-spec foo() -> erlang_bar:t().", c) ==
               ~s|foo() -> <a href="erlang_bar.html#t:t/0">erlang_bar:t</a>().|
    end

    test "OTP type", c do
      assert autolink_spec(~S"-spec foo() -> sets:set().", c) ==
               ~s|foo() -> <a href="https://www.erlang.org/doc/man/sets.html#type-set">sets:set</a>().|
    end

    test "OTP private type", c do
      assert autolink_spec(~S"-spec foo() -> array:array_indx().", c) ==
               ~s|foo() -> <a href="https://www.erlang.org/doc/man/array.html#type-array_indx">array:array_indx</a>().|
    end

    test "skip typespec name", c do
      assert autolink_spec(~S"-spec t() -> t().", c) ==
               ~s|t() -> <a href="#t:t/0">t</a>().|
    end

    test "same spec and type name", c do
      assert autolink_spec(~S"-spec t(t()) -> t().", c) ==
               ~s|t(<a href="#t:t/0">t</a>()) -> <a href="#t:t/0">t</a>().|
    end

    test "non-standard name", c do
      assert autolink_spec(~S"-spec 'Foo'() -> ok.", c) ==
               ~s|'Foo'() -> ok.|
    end

    test "bad remote type", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_spec(~S"-spec foo() -> bad:bad(atom()).", c) ==
                        ~s|foo() -> bad:bad(atom()).|
             end) =~ ~r{references type "bad:bad/1" but it is undefined or private}
    end
  end

  defp autolink_doc(doc, opts \\ [], c) do
    fixtures(c, doc)

    {:docs_v1, _, _, "application/erlang+html", %{"en" => doc}, _, _} =
      Code.fetch_docs(:erlang_foo)

    doc
    |> ExDoc.DocAST.parse!("application/erlang+html")
    |> do_autolink_doc(opts)
  end

  defp do_autolink_doc(doc, opts \\ []) do
    doc
    |> ExDoc.Language.Erlang.autolink_doc(opts)
    |> ExDoc.DocAST.to_string()
  end

  defp autolink_spec(binary, opts \\ [], c) when is_binary(binary) do
    opts =
      opts
      |> Keyword.put_new(:current_module, :erlang_foo)

    fixtures(c, "")
    {:ok, tokens, _} = :erl_scan.string(String.to_charlist(binary))
    {:ok, ast} = :erl_parse.parse_form(tokens)
    ExDoc.Language.Erlang.autolink_spec(ast, opts)
  end

  defp autolink_extra(text, c) do
    # Markdown is usually not valid EDoc
    fixtures(c, "")

    [{:p, _, [ast], _}] = ExDoc.Markdown.to_ast(text, [])

    do_autolink_doc(ast)
  end

  defp fixtures(c, doc) do
    erlc(c, :erlang_foo, """
    %% @doc
    %% #{doc}
    -module(erlang_foo).
    -export([foo/0]).
    -export_type([t/0, opaque_t/0]).
    -type t() :: atom().
    -type opaque_t() :: atom().
    foo() -> ok.
    """)

    erlc(c, :erlang_bar, """
    -module(erlang_bar).
    -export([bar/0]).
    -export_type([t/0]).
    -type t() :: atom().
    bar() -> ok.
    """)
  end
end
