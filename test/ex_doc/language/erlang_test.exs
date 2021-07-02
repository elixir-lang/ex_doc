defmodule ExDoc.Language.ErlangTest do
  use ExUnit.Case
  import TestHelper

  setup :create_tmp_dir

  @moduletag :otp24

  describe "autolink_doc/2" do
    test "module", c do
      assert autolink_doc("{@link bar}", c) ==
               ~s{<a href="bar.html"><code>bar</code></a>}
    end

    test "OTP module", c do
      assert autolink_doc("{@link array}", c) ==
               ~s{<a href="https://erlang.org/doc/man/array.html"><code>array</code></a>}
    end

    test "external module", c do
      assert autolink_doc("{@link 'Elixir.EarmarkParser'}", c) ==
               ~s{<a href="https://hexdocs.pm/earmark_parser/EarmarkParser.html"><code>'Elixir.EarmarkParser'</code></a>}
    end

    test "external module - extension is ignored", c do
      assert autolink_doc("{@link 'Elixir.EarmarkParser'}", [ext: ".xhtml"], c) ==
               ~s{<a href="https://hexdocs.pm/earmark_parser/EarmarkParser.html"><code>'Elixir.EarmarkParser'</code></a>}
    end

    test "custom text", c do
      assert autolink_doc("{@link array. The <code>array</code> module}", c) ==
               ~s{<a href="https://erlang.org/doc/man/array.html">The <code>array</code> module</a>}
    end

    test "local function", c do
      assert autolink_doc("{@link foo/0}", [current_module: :foo], c) ==
               ~s{<a href="#foo/0"><code>foo/0</code></a>}
    end

    test "remote function", c do
      assert autolink_doc("{@link bar:bar/0}", c) ==
               ~s{<a href="bar.html#bar/0"><code>bar:bar/0</code></a>}
    end

    test "OTP function", c do
      assert autolink_doc("{@link array:new/0}", c) ==
               ~s{<a href="https://erlang.org/doc/man/array.html#new-0"><code>array:new/0</code></a>}
    end

    test "ERTS function", c do
      assert autolink_doc("{@link zlib:gunzip/1}", c) ==
               ~s{<a href="https://erlang.org/doc/man/zlib.html#gunzip-1"><code>zlib:gunzip/1</code></a>}
    end

    test "external function", c do
      assert autolink_doc("{@link 'Elixir.EarmarkParser':as_ast/2}", c) ==
               ~s{<a href="https://hexdocs.pm/earmark_parser/EarmarkParser.html#as_ast/2"><code>'Elixir.EarmarkParser':as_ast/2</code></a>}
    end

    test "local type", c do
      assert autolink_doc("{@link t()}", [current_module: :foo], c) ==
               ~s{<a href="#t:t/0"><code>t()</code></a>}
    end

    test "remote type", c do
      assert autolink_doc("{@link bar:t()}", c) ==
               ~s{<a href="bar.html#t:t/0"><code>bar:t()</code></a>}
    end

    test "OTP type", c do
      assert autolink_doc("{@link array:array()}", c) ==
               ~s{<a href="https://erlang.org/doc/man/array.html#type-array"><code>array:array()</code></a>}
    end

    test "bad module", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_doc("{@link bad}", c) == ~s{<code>bad</code>}
             end) =~ "references module \"bad\" but it is undefined"
    end

    test "bad local function", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_doc("{@link bad/0}", c) == ~s{<code>bad/0</code>}
             end) =~ "references \"bad/0\" but it is undefined or private"
    end

    test "bad remote function", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_doc("{@link bad:bad/0}", c) == ~s{<code>bad:bad/0</code>}
             end) =~ "references \"bad:bad/0\" but it is undefined or private"
    end

    test "bad local type", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_doc("{@link bad()}", c) == ~s{<code>bad()</code>}
             end) =~ "references \"bad\(\)\" but it is undefined or private"
    end

    test "bad remote type", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_doc("{@link bad:bad()}", c) == ~s{<code>bad:bad()</code>}
             end) =~ "references \"bad:bad()\" but it is undefined or private"
    end

    test "application", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_doc("{@link //foo}", c) == ~s{<code>foo</code>}
             end) =~ ~r{application references are not supported: //foo}
    end

    test "application module", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_doc("{@link //foo/bar}", c) == ~s{<code>bar</code>}
             end) =~ ~r{application references are not supported: //foo/bar}
    end

    test "application function", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_doc("{@link //foo/bar:baz/0}", c) == ~s{<code>bar:baz/0</code>}
             end) =~ ~r{application references are not supported: //foo/bar:baz/0}
    end

    test "application type", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_doc("{@link //foo/bar:baz()}", c) == ~s{<code>bar:baz()</code>}
             end) =~ ~r{application references are not supported: //foo/bar:baz\(\)}
    end
  end

  describe "autolink_spec/2" do
    test "spec", c do
      assert autolink_spec("-spec foo() -> atom().", c) ==
               ~s|foo() -> atom().|
    end

    test "callback", c do
      assert autolink_spec("-callback foo() -> atom().", c) ==
               ~s|foo() -> atom().|
    end

    test "type", c do
      assert autolink_spec("-type foo() :: atom().", c) ==
               ~s|foo() :: atom().|
    end

    test "opaque", c do
      assert autolink_spec("-opaque foo() :: opaque_t().", [current_module: :foo], c) ==
               ~s|foo() :: <a href="#t:opaque_t/0">opaque_t</a>().|
    end

    test "tuple", c do
      assert autolink_spec(~S"-spec foo() -> {ok, atom()}.", c) ==
               ~S|foo() -> {ok, atom()}.|
    end

    test "list", c do
      assert autolink_spec(~S"-spec foo() -> [atom()].", c) ==
               ~S|foo() -> [atom()].|
    end

    test "map", c do
      assert autolink_spec(~S"-spec foo() -> #{atom() := string(), float() => integer()}.", c) ==
               ~S|foo() -> #{atom() := string(), float() => integer()}.|
    end

    test "vars", c do
      assert autolink_spec(~s"-spec foo(X) -> {Y :: atom(), X}.", c) ==
               ~s|foo(X) -> {Y :: atom(), X}.|
    end

    test "union", c do
      assert autolink_spec(~s"-spec foo() -> ok | error.", c) ==
               ~s[foo() -> ok | error.]
    end

    test "record", c do
      assert autolink_spec(~s"-spec foo() -> #file_info{}.", c) ==
               ~s[foo() -> #file_info{}.]
    end

    test "local type", c do
      assert autolink_spec(~S"-spec foo() -> t().", [current_module: :foo], c) ==
               ~s|foo() -> <a href="#t:t/0">t</a>().|
    end

    test "remote type", c do
      assert autolink_spec(~S"-spec foo() -> bar:t().", c) ==
               ~s|foo() -> <a href="bar.html#t:t/0">bar:t</a>().|
    end

    test "OTP type", c do
      assert autolink_spec(~S"-spec foo() -> sets:set().", c) ==
               ~s|foo() -> <a href="https://erlang.org/doc/man/sets.html#type-set">sets:set</a>().|
    end

    test "skip typespec name", c do
      assert autolink_spec(~S"-spec t() -> t().", [current_module: :foo], c) ==
               ~s|t() -> <a href="#t:t/0">t</a>().|
    end

    test "non-standard name", c do
      assert autolink_spec(~S"-spec 'Foo'() -> ok.", c) ==
               ~s|'Foo'() -> ok.|
    end

    test "bad remote type", c do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert autolink_spec(~S"-spec foo() -> bad:bad(atom()).", c) ==
                        ~s|foo() -> bad:bad(atom()).|
             end) =~ ~r{references "bad:bad/1" but it is undefined or private}
    end
  end

  defp autolink_doc(doc, opts \\ [], c) do
    fixtures(c, doc)
    {:docs_v1, _, _, "application/erlang+html", %{"en" => doc}, _, _} = Code.fetch_docs(:foo)

    doc
    |> ExDoc.DocAST.parse!("application/erlang+html")
    |> ExDoc.Language.Erlang.autolink_doc(opts)
    |> ExDoc.DocAST.to_string()
  end

  defp autolink_spec(binary, opts \\ [], c) when is_binary(binary) do
    fixtures(c, "")
    {:ok, tokens, _} = :erl_scan.string(String.to_charlist(binary))
    {:ok, ast} = :erl_parse.parse_form(tokens)
    ExDoc.Language.Erlang.autolink_spec(ast, opts)
  end

  defp fixtures(c, doc) do
    erlc(c, :foo, """
    %% @doc
    %% #{doc}
    -module(foo).
    -export([foo/0]).
    -export_type([t/0, opaque_t/0]).
    -type t() :: atom().
    -type opaque_t() :: atom().
    foo() -> ok.
    """)

    erlc(c, :bar, """
    -module(bar).
    -export([bar/0]).
    -export_type([t/0]).
    -type t() :: atom().
    bar() -> ok.
    """)
  end
end
