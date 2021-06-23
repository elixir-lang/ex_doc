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

    @tag :skip
    test "external module", c do
      assert autolink_doc("{@link 'Elixir.EarmarkParser':as_ast/2}", c) ==
               ~s{<a href="https://hexdocs.pm/earmark_parser/EarmarkParser.html#as_ast/2"><code>'Elixir.EarmarkParser':as_ast/2</code></a>}
    end

    test "custom text", c do
      assert autolink_doc("{@link array. The <code>array</code> module}", c) ==
               ~s{<a href="https://erlang.org/doc/man/array.html">The <code>array</code> module</a>}
    end

    test "local function", c do
      assert autolink_doc("{@link foo/0}", c) ==
               ~s{<a href="#foo/0"><code>foo/0</code></a>}
    end

    test "remote function", c do
      assert autolink_doc("{@link bar:bar/0}", c) ==
               ~s{<a href="bar.html#bar-0"><code>bar:bar/0</code></a>}
    end

    test "OTP function", c do
      assert autolink_doc("{@link array:new/0}", c) ==
               ~s{<a href="https://erlang.org/doc/man/array.html#new-0"><code>array:new/0</code></a>}
    end

    test "ERTS function", c do
      assert autolink_doc("{@link zlib:gunzip/1}", c) ==
               ~s{<a href="https://erlang.org/doc/man/zlib.html#gunzip-1"><code>zlib:gunzip/1</code></a>}
    end

    test "local type", c do
      assert autolink_doc("{@link t()}", c) ==
               ~s{<a href="#t:t/0"><code>t()</code></a>}
    end

    test "remote type", c do
      assert autolink_doc("{@link bar:bar()}", c) ==
               ~s{<a href="bar.html#type-bar"><code>bar:bar()</code></a>}
    end

    test "OTP type", c do
      assert autolink_doc("{@link array:array()}", c) ==
               ~s{<a href="https://erlang.org/doc/man/array.html#type-array"><code>array:array()</code></a>}
    end

    # TODO:
    # test "bad module", c do
    #   assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
    #            assert autolink_doc("{@link bad}", c) == ~s{<code>bad</code>}
    #          end) =~ "bad"
    # end

    # test "bad local function", c do
    #   assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
    #            assert autolink_doc("{@link bad/0}", c) == ~s{<code>bad/0</code>}
    #          end) =~ "bad/0"
    # end

    # test "bad remote function", c do
    #   assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
    #            assert autolink_doc("{@link bad:bad/0}", c) == ~s{<code>bad:bad/0</code>}
    #          end) =~ "bad:bad/0"
    # end

    # test "bad local type", c do
    #   assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
    #            assert autolink_doc("{@link bad()}", c) == ~s{<code>bad()</code>}
    #          end) =~ "bad()"
    # end

    # test "bad remote type", c do
    #   assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
    #            assert autolink_doc("{@link bad:bad()}", c) == ~s{<code>bad:bad()</code>}
    #          end) =~ "bad:bad()"
    # end

    # test "application", c do
    #   assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
    #            assert autolink_doc("{@link //foo}", c) == ~s{<code>foo</code>}
    #          end) =~ ~r{application references are not supported: //foo}
    # end

    # test "application module", c do
    #   assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
    #            assert autolink_doc("{@link //foo/bar}", c) == ~s{<code>bar</code>}
    #          end) =~ ~r{application references are not supported: //foo/bar}
    # end

    # test "application function", c do
    #   assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
    #            assert autolink_doc("{@link //foo/bar:baz/0}", c) == ~s{<code>bar:baz/0</code>}
    #          end) =~ ~r{application references are not supported: //foo/bar:baz/0}
    # end

    # test "application type", c do
    #   assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
    #            assert autolink_doc("{@link //foo/bar:baz()}", c) == ~s{<code>bar:baz()</code>}
    #          end) =~ ~r{application references are not supported: //foo/bar:baz\(\)}
    # end
  end

  defp autolink_doc(doc, opts \\ [], c) do
    erlc(c, :foo, """
    %% @doc
    %% #{doc}
    -module(foo).
    -export([foo/0]).
    -export_type([t/0]).
    -type t() :: atom().
    foo() -> ok.
    """)

    :ok = edoc_to_chunk(:foo)

    erlc(c, :bar, """
    -module(bar).
    -export([bar/0]).
    -export_type([t/0]).
    -type t() :: atom().
    bar() -> ok.
    """)

    :ok = edoc_to_chunk(:bar)

    {:docs_v1, _, _, "application/erlang+html", %{"en" => doc}, _, _} = Code.fetch_docs(:foo)

    opts =
      opts
      |> Keyword.put_new(:file, "nofile")

    doc
    |> ExDoc.DocAST.parse!("application/erlang+html")
    |> ExDoc.Language.Erlang.autolink_doc(opts)
    |> ExDoc.DocAST.to_string()
  end
end
