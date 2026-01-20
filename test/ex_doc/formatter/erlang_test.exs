defmodule ExDoc.Formatter.ErlangTest do
  use ExUnit.Case, async: true
  import TestHelper

  @moduletag :tmp_dir

  @tag :otp_eep48
  test "EEP 48", c do
    erlc(c, :foo, ~S"""
    %% @doc
    %% foo module.
    -module(foo).
    -export([foo/1, bar/0]).
    -export_type([t/0, t2/0]).

    %% @doc
    %% f/0 function.
    -spec foo(t()) -> t().
    foo(X) -> X.

    -spec bar() -> baz.
    bar() -> baz.

    -type t() :: atom().
    %% t/0 type.

    -record(rec, {k1 :: any(), k2 :: any()}).

    -type t2() :: #rec{k1 :: uri_string:uri_string(), k2 :: uri_string:uri_string() | undefined}.
    """)

    generate(c)
    html = c.tmp_dir |> Path.join("doc/foo.html") |> File.read!()

    # Check specs are rendered with proper links in HTML
    assert html =~
             ~s|-spec</span> foo(<a href="#t:t/0">t</a>()) -> <a href="#t:t/0">t</a>().|

    assert html =~
             ~s|-spec</span> bar() -> baz.|

    # Check types are rendered with proper links in HTML
    assert html =~
             ~s|-type</span> t() :: <a href="https://www.erlang.org/doc/apps/erts/erlang.html#t:atom/0">atom</a>().|

    assert html =~
             ~s|-type</span> t2() :: #rec{k1 :: <a href="https://www.erlang.org/doc/apps/stdlib/uri_string.html#t:uri_string/0">uri_string:uri_string</a>(), k2 :: <a href="https://www.erlang.org/doc/apps/stdlib/uri_string.html#t:uri_string/0">uri_string:uri_string</a>() \| undefined}.|

    # EEP 48 uses erlang+html format, so no markdown should be generated
    refute File.exists?(Path.join(c.tmp_dir, "doc/foo.md"))
  end

  @tag :otp_eep59
  test "EEP 59", c do
    erlc(c, :bar, ~S"""
    -module(bar).
    -moduledoc("bar module.").
    -export([bar/1, baz/0]).
    -export_type([t/0]).

    -doc("bar/1 function.").
    -spec bar(t()) -> t().
    bar(X) -> X.

    -doc("baz/0 function.").
    -spec baz() -> atom().
    baz() -> ok.

    -doc("t/0 type.").
    -type t() :: atom().
    """)

    generate(c)
    html = c.tmp_dir |> Path.join("doc/bar.html") |> File.read!()

    # Check moduledoc is rendered in HTML
    assert html =~ "<p>bar module.</p>"

    # Check function docs are rendered in HTML
    assert html =~ "<p>bar/1 function.</p>"
    assert html =~ "<p>baz/0 function.</p>"

    # Check specs are rendered with proper links in HTML
    assert html =~
             ~s|-spec</span> bar(<a href="#t:t/0">t</a>()) -> <a href="#t:t/0">t</a>().|

    assert html =~
             ~s|-spec</span> baz() -> <a href="https://www.erlang.org/doc/apps/erts/erlang.html#t:atom/0">atom</a>().|

    # Check type is rendered with proper links in HTML
    assert html =~
             ~s|-type</span> t() :: <a href="https://www.erlang.org/doc/apps/erts/erlang.html#t:atom/0">atom</a>().|

    # Check type doc is rendered in HTML
    assert html =~ "<p>t/0 type.</p>"

    # EEP 59 uses text/markdown format, so markdown should be generated
    markdown = c.tmp_dir |> Path.join("doc/bar.md") |> File.read!()

    # Check moduledoc in markdown
    assert markdown =~ "bar module."

    # Check function docs in markdown
    assert markdown =~ "bar/1 function."
    assert markdown =~ "baz/0 function."

    # Check specs in markdown
    assert markdown =~ "-spec bar(t()) -> t()."
    assert markdown =~ "-spec baz() -> atom()."

    # Check type in markdown
    assert markdown =~ "-type t() :: atom()."
    assert markdown =~ "t/0 type."
  end

  defp generate(c) do
    config = [
      version: "1.0.0",
      project: "Foo",
      formatters: ["html", "markdown"],
      output: Path.join(c.tmp_dir, "doc"),
      source_beam: Path.join(c.tmp_dir, "ebin"),
      extras: []
    ]

    ExDoc.generate(config[:project], config[:version], [config[:source_beam]], config)
  end
end
