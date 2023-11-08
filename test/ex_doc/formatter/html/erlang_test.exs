defmodule ExDoc.Formatter.HTML.ErlangTest do
  use ExUnit.Case
  import TestHelper

  @moduletag :otp_eep48
  @moduletag :tmp_dir

  test "smoke test", c do
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

    doc = generate_docs(c)
    html = Floki.raw_html(doc)

    assert html =~
             ~s|-spec</span> foo(<a href=\"#t:t/0\">t</a>()) -&gt; <a href=\"#t:t/0\">t</a>().|

    assert html =~
             ~s|-type</span> t() :: <a href=\"https://www.erlang.org/doc/man/erlang.html#type-atom\">atom</a>().|

    assert html =~
             ~s|-type</span> t2() :: #rec{k1 :: <a href=\"https://www.erlang.org/doc/man/uri_string.html#type-uri_string\">uri_string:uri_string</a>(), k2 :: <a href=\"https://www.erlang.org/doc/man/uri_string.html#type-uri_string\">uri_string:uri_string</a>() \| undefined}.|
  end

  defp generate_docs(c) do
    config = [
      version: "1.0.0",
      project: "Foo",
      formatter: "html",
      output: Path.join(c.tmp_dir, "doc"),
      source_beam: Path.join(c.tmp_dir, "ebin"),
      extras: []
    ]

    ExDoc.generate_docs(config[:project], config[:version], config)

    [c.tmp_dir, "doc", "foo.html"] |> Path.join() |> File.read!() |> Floki.parse_document!()
  end
end
