defmodule ExDoc.Language.ErlangTest do
  use ExUnit.Case, async: false
  import TestHelper

  @moduletag :otp_has_docs
  @moduletag :tmp_dir

  defp sigil_m(text, []) do
    ExDoc.Markdown.to_ast(text, [])
  end

  describe "autolink_doc/2 for edoc" do
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
               ~s|<a href="#new/0"><code>array:new/0</code></a>|
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
             end) =~ ~s|references type "t:bad/0" but it is undefined or private|
    end

    test "bad remote type", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_doc("{@link bad:bad()}", c) == ~s|<code>bad:bad()</code>|
             end) =~ ~s|references type "t:bad:bad/0" but it is undefined or private|
    end

    test "application", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_doc("{@link //foo}", c) == ~s|<code>//foo</code>|
             end) =~ ~r{invalid reference: foo:index}
    end
  end

  describe "autolink_doc/2 for markdown" do
    test "module in module code", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_markdown("`erlang_bar`", c) ==
                        ~s|<code class="inline">erlang_bar</code>|
             end) == ""
    end

    test "m:module in module code", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_markdown("`m:erlang_bar`", c) ==
                        ~s|<a href=\"erlang_bar.html\"><code class="inline">erlang_bar</code></a>|
             end) == ""
    end

    test "module in module code reference", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_markdown("[`erlang_bar`](`erlang_bar`)", c) ==
                        ~s|<a href=\"erlang_bar.html\"><code class="inline">erlang_bar</code></a>|
             end) == ""
    end

    test "function in module code", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_markdown("`foo/0`", c) ==
                        ~s|<a href=\"#foo/0\"><code class="inline">foo/0</code></a>|
             end) == ""
    end

    test "function in module ref", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_markdown("[`foo/0`](`foo/0`)", c) ==
                        ~s|<a href="#foo/0"><code class="inline">foo/0</code></a>|
             end) == ""
    end

    test "escaped function in module", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_markdown(~S"`\c:ls/0`", c, deps: [stdlib: "stdlib/"]) ==
                        ~s|<a href="stdlib/c.html#ls/0"><code class="inline">c:ls/0</code></a>|
             end) == ""
    end

    ## There is a bug in EarmarkParser that leaves a trailing `)` in when parsing the
    ## markdown below. We expect the bug to be present right now so that we can update th
    ## testcase when it is fixed. See https://github.com/RobertDober/earmark_parser/issues/139
    ## for details about the bug.
    test "escaped function in module ref", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_markdown(~S"[c](`\\c:ls/0`)", c, deps: [stdlib: "stdlib/"]) ==
                        ~s|<a href="stdlib/c.html#ls/0">c</a>)|
             end) == ""
    end

    test "function in module autoimport", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_markdown("`node()`", c) ==
                        ~s|<code class="inline">node()</code>|
             end) == ""
    end

    test "function in module autoimport using slash", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_markdown("`node/0`", c) ==
                        ~s|<a href="https://www.erlang.org/doc/man/erlang.html#node-0"><code class="inline">node/0</code></a>|
             end) == ""
    end

    test "type in module autoimport", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_markdown("`t:integer()`", c) ==
                        ~s|<code class="inline">t:integer()</code>|
             end) == ""
    end

    test "type in module autoimport using slash", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_markdown("`t:integer/0`", c) ==
                        ~s|<a href="https://www.erlang.org/doc/man/erlang.html#type-integer"><code class="inline">integer/0</code></a>|
             end) == ""
    end

    test "bad function in module code", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_markdown("`bad/0`", c) ==
                        ~s|<code class="inline">bad/0</code>|
             end) == ""
    end

    test "bad function in module ref", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_markdown("[Bad](`bad/0`)", c) ==
                        ~s|Bad|
             end) =~ ~s|documentation references function "bad/0" but it is undefined or private|
    end

    test "linking to auto-imported nil works", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_markdown("[`[]`](`t:nil/0`)", c) ==
                        ~s|<a href="https://www.erlang.org/doc/man/erlang.html#type-nil"><code class="inline">[]</code></a>|
             end) == ""
    end

    test "linking to local nil works", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_markdown(
                        "[`[]`](`t:nil/0`)",
                        c,
                        extra_foo_code: "-export_type([nil/0]).\n-type nil() :: [].\n"
                      ) ==
                        ~s|<a href="#t:nil/0"><code class="inline">[]</code></a>|
             end) == ""
    end

    test "linking to local nil function works", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_markdown(
                        "[`nil`](`nil/0`)",
                        c,
                        extra_foo_code: "-export([nil/0]).\nnil() -> [].\n"
                      ) ==
                        ~s|<a href="#nil/0"><code class="inline">nil</code></a>|
             end) == ""
    end

    test "linking to exported nil function works", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_markdown(
                        "[`nil`](`erlang_bar:nil/0`)",
                        c,
                        extra_bar_code: "-export([nil/0]).\nnil() -> [].\n"
                      ) ==
                        ~s|<a href="erlang_bar.html#nil/0"><code class="inline">nil</code></a>|
             end) == ""
    end

    test "linking to local is_integer function works", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_markdown(
                        "[`is_integer`](`is_integer/1`)",
                        c,
                        extra_foo_code:
                          "-export([is_integer/1]).\nis_integer(I) -> erlang:is_integer(I).\n"
                      ) ==
                        ~s|<a href="#is_integer/1"><code class="inline">is_integer</code></a>|
             end) == ""
    end

    test "linking to extra works", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_markdown(
                        "[extra](`e:foolib:extra.md`)",
                        c
                      ) ==
                        ~s|<a href="https://foolib.com/extra.html">extra</a>|
             end) == ""
    end

    test "linking to extra anchor works", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_markdown(
                        "[extra](`e:foolib:extra.md#anchor`)",
                        c
                      ) ==
                        ~s|<a href="https://foolib.com/extra.html#anchor">extra</a>|
             end) == ""
    end

    test "linking to extra xhtml works", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_markdown(
                        "[extra](`e:foolib:extra.xhtml`)",
                        c
                      ) ==
                        ~s|<a href="https://foolib.com/extra.xhtml">extra</a>|
             end) == ""
    end

    test "linking to local extra works does not work", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_markdown(
                        "[extra](`e:extra.md`)",
                        c
                      ) ==
                        ~s|extra|
             end) =~ ~r/documentation references "e:extra.md" but it is invalid/
    end
  end

  describe "autolink_doc/2 for extra" do
    test "function", c do
      assert autolink_extra("`erlang_foo:foo/0`", c) ==
               ~s|<a href="erlang_foo.html#foo/0"><code class="inline">erlang_foo:foo/0</code></a>|
    end

    test "OTP function", c do
      assert autolink_extra("`lists:reverse/1`", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/lists.html#reverse-1"><code class="inline">lists:reverse/1</code></a>|
    end

    test "type", c do
      assert autolink_extra("`t:erlang_bar:t/0`", c) ==
               ~s|<a href="erlang_bar.html#t:t/0"><code class="inline">erlang_bar:t/0</code></a>|
    end

    test "OTP type", c do
      assert autolink_extra("`t:array:array/0`", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/array.html#type-array"><code class="inline">array:array/0</code></a>|
    end

    test "module", c do
      assert autolink_extra("`m:erlang_foo`", c) ==
               ~s|<a href="erlang_foo.html"><code class="inline">erlang_foo</code></a>|
    end

    test "OTP module", c do
      assert autolink_extra("`m:rpc`", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/rpc.html"><code class="inline">rpc</code></a>|
    end

    test "bad module function", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_extra("`bad:bad/0`", c) ==
                        ~s|<code class="inline">bad:bad/0</code>|
             end) =~ ~s||
    end

    test "bad function", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_extra("`bad/0`", c) ==
                        ~s|<code class="inline">bad/0</code>|
             end) == ""
    end

    test "invalid function", c do
      assert autolink_extra("`...a/0`", c) ==
               ~s|<code class="inline">...a/0</code>|
    end

    test "bad type", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_extra("`t:bad:bad/0`", c) ==
                        ~s|<code class="inline">t:bad:bad/0</code>|
             end) =~ ~s||
    end

    test "bad module", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_extra("`does_not_exist`", c) ==
                        ~s|<code class="inline">does_not_exist</code>|
             end) == ""
    end

    test "bad module using m:", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_extra("`m:does_not_exist`", c) ==
                        ~s|<code class="inline">m:does_not_exist</code>|
             end) =~ ~r|documentation references module \"does_not_exist\" but it is undefined|
    end

    test "extras" do
      opts = [
        extras: %{"Foo Bar.md" => "foo-bar", "Bar Baz.livemd" => "bar-baz"},
        language: ExDoc.Language.Erlang
      ]

      assert ExDoc.Language.Erlang.autolink_doc(~m"[Foo](Foo Bar.md)", opts) ==
               ~m"[Foo](foo-bar.html)"

      assert ExDoc.Language.Erlang.autolink_doc(~m"[Bar](Bar Baz.livemd)", opts) ==
               ~m"[Bar](bar-baz.html)"

      assert ExDoc.Language.Erlang.autolink_doc(~m"[Foo](Foo Bar.md)", [ext: ".xhtml"] ++ opts) ==
               ~m"[Foo](foo-bar.xhtml)"

      assert ExDoc.Language.Erlang.autolink_doc(~m"[Foo](Foo Bar.md#baz)", opts) ==
               ~m"[Foo](foo-bar.html#baz)"

      assert ExDoc.Language.Erlang.autolink_doc(~m"[Foo](../guide/Foo Bar.md)", opts) ==
               ~m"[Foo](foo-bar.html)"

      assert ExDoc.Language.Erlang.autolink_doc(~m"[Foo](http://example.com/foo.md)", opts) ==
               ~m"[Foo](http://example.com/foo.md)"

      assert ExDoc.Language.Erlang.autolink_doc(~m"[Foo](#baz)", opts) == ~m"[Foo](#baz)"
    end
  end

  describe "autolink_spec/2" do
    test "spec", c do
      assert autolink_spec("-spec foo() -> t().", c) ==
               ~s|foo() -> <a href="#t:t/0">t</a>().|
    end

    test "spec when fun is called record", c do
      assert autolink_spec("-spec record(module()) -> [[{module(), atom()}]].", c) ==
               ~s|record(<a href="https://www.erlang.org/doc/man/erlang.html#type-module">module</a>()) -> [[{<a href="https://www.erlang.org/doc/man/erlang.html#type-module">module</a>(), <a href="https://www.erlang.org/doc/man/erlang.html#type-atom">atom</a>()}]].|
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
               ~S|foo() -> #{<a href="https://www.erlang.org/doc/man/erlang.html#type-atom">atom</a>() := <a href="https://www.erlang.org/doc/man/erlang.html#type-string">string</a>(), <a href="https://www.erlang.org/doc/man/erlang.html#type-float">float</a>() => <a href="#t:t/0">t</a>()}.|
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
               ~s[foo() -> #x{x :: <a href="https://www.erlang.org/doc/man/erlang.html#type-atom">atom</a>()} | <a href="#t:t/0">t</a>().]
    end

    test "record - two fields", c do
      assert autolink_spec(~s"-spec foo() -> #x{x :: atom(), y :: integer()} | t().", c) ==
               ~s[foo() -> #x{x :: <a href="https://www.erlang.org/doc/man/erlang.html#type-atom">atom</a>(), y :: <a href="https://www.erlang.org/doc/man/erlang.html#type-integer">integer</a>()} | <a href="#t:t/0">t</a>().]
    end

    test "record - two fields, known types", c do
      assert autolink_spec(~s"-spec foo() -> #x{x :: [t()], y :: t()}.", c) ==
               ~s|foo() -> #x{x :: [<a href="#t:t/0">t</a>()], y :: <a href="#t:t/0">t</a>()}.|
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

    test "prefixed name", c do
      assert autolink_spec(~S"-spec erlang_foo:t() -> ok.", c) ==
               ~s|erlang_foo:t() -> ok.|
    end

    test "bad remote type", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_spec(~S"-spec foo() -> bad:bad(atom()).", c) ==
                        ~s|foo() -> bad:bad(<a href="https://www.erlang.org/doc/man/erlang.html#type-atom">atom</a>()).|
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
    |> ExDoc.Language.Erlang.autolink_doc(opts ++ [language: ExDoc.Language.Erlang])
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

    res =
      do_autolink_doc(ast)

    res
  end

  defp autolink_markdown(text, c, opts \\ []) do
    # Markdown is usually not valid EDoc
    fixtures(c, "", opts)

    ast =
      case ExDoc.Markdown.to_ast(text, []) do
        [{:p, _, ast, _}] -> ast
        ast -> ast
      end

    res =
      do_autolink_doc(
        ast,
        [
          current_module: :erlang_foo,
          module_id: "erlang_foo",
          deps: [foolib: "https://foolib.com"]
        ] ++
          Keyword.drop(opts, [:extra_foo_code, :extra_bar_code])
      )

    res
  end

  defp fixtures(c, doc, opts \\ []) do
    erlc(c, :erlang_foo, """
    %% @doc
    %% #{doc}
    -module(erlang_foo).
    -export([foo/0]).
    -export_type([t/0, opaque_t/0]).
    -type t() :: atom().
    -type opaque_t() :: atom().
    #{opts[:extra_foo_code]}
    foo() -> ok.
    """)

    erlc(c, :erlang_bar, """
    -module(erlang_bar).
    -export([bar/0]).
    -export_type([t/0]).
    -type t() :: atom().
    #{opts[:extra_bar_code]}
    bar() -> ok.
    """)
  end
end
