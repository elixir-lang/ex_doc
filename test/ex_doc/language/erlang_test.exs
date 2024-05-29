defmodule ExDoc.Language.ErlangTest do
  # ExDoc.Refs is global
  use ExUnit.Case, async: false

  import TestHelper

  @moduletag :otp_has_docs
  @moduletag :tmp_dir

  describe "autolink_doc/2 for edoc" do
    test "module", c do
      assert autolink_edoc("{@link erlang_bar}", c) ==
               ~s|<a href="erlang_bar.html"><code>erlang_bar</code></a>|
    end

    test "current module", c do
      assert autolink_edoc("{@link erlang_foo}", c, current_module: :erlang_foo) ==
               ~s|<a href="erlang_foo.html#content"><code>erlang_foo</code></a>|
    end

    test "OTP module", c do
      assert autolink_edoc("{@link array}", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/array.html"><code>array</code></a>|
    end

    test "OTP module when generating OTP docs", c do
      assert autolink_edoc("{@link array}", c, deps: [stdlib: "https://example.com/stdlib"]) ==
               ~s|<a href="https://example.com/stdlib/array.html"><code>array</code></a>|
    end

    test "app module", c do
      assert autolink_edoc("{@link //stdlib/array}", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/array.html"><code>array</code></a>|
    end

    @tag warnings: :send
    test "app", c do
      assert warn(fn ->
               assert autolink_edoc("{@link //stdlib. `stdlib'}", c) ==
                        ~s|<code>stdlib</code>|
             end) =~ ~s|invalid reference: stdlib:index (seeapp)|
    end

    test "external module", c do
      assert autolink_edoc("{@link 'Elixir.EarmarkParser'}", c) ==
               ~s|<a href="https://hexdocs.pm/earmark_parser/EarmarkParser.html"><code>'Elixir.EarmarkParser'</code></a>|
    end

    test "external module - extension is ignored", c do
      assert autolink_edoc("{@link 'Elixir.EarmarkParser'}", c, ext: ".xhtml") ==
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
      assert autolink_edoc("{@link array. The <code>array</code> module}", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/array.html">The <code>array</code> module</a>|
    end

    test "local function", c do
      assert autolink_edoc("{@link foo/0}", c, current_module: :erlang_foo) ==
               ~s|<a href="#foo/0"><code>foo/0</code></a>|
    end

    test "remote function", c do
      assert autolink_edoc("{@link erlang_bar:bar/0}", c) ==
               ~s|<a href="erlang_bar.html#bar/0"><code>erlang_bar:bar/0</code></a>|
    end

    test "OTP function", c do
      assert autolink_edoc("{@link array:new/0}", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/array.html#new/0"><code>array:new/0</code></a>|
    end

    test "OTP function when generating OTP docs", c do
      assert autolink_edoc("{@link array:new/0}", c, apps: [:stdlib]) ==
               ~s|<a href="array.html#new/0"><code>array:new/0</code></a>|
    end

    test "OTP function when generating OTP docs, same module", c do
      assert autolink_edoc("{@link array:new/0}", c, current_module: :array, apps: [:stdlib]) ==
               ~s|<a href="#new/0"><code>array:new/0</code></a>|
    end

    test "ERTS function", c do
      assert autolink_edoc("{@link zlib:gunzip/1}", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/zlib.html#gunzip/1"><code>zlib:gunzip/1</code></a>|
    end

    test "app function", c do
      assert autolink_edoc("{@link //stdlib/array:new/0}", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/array.html#new/0"><code>array:new/0</code></a>|
    end

    test "external function", c do
      assert autolink_edoc("{@link 'Elixir.EarmarkParser':as_ast/2}", c) ==
               ~s|<a href="https://hexdocs.pm/earmark_parser/EarmarkParser.html#as_ast/2"><code>'Elixir.EarmarkParser':as_ast/2</code></a>|
    end

    test "local type", c do
      assert autolink_edoc("{@link t()}", c, current_module: :erlang_foo) ==
               ~s|<a href="#t:t/0"><code>t()</code></a>|
    end

    test "remote type", c do
      assert autolink_edoc("{@link erlang_bar:t()}", c) ==
               ~s|<a href="erlang_bar.html#t:t/0"><code>erlang_bar:t()</code></a>|
    end

    test "OTP type", c do
      assert autolink_edoc("{@link array:array()}", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/array.html#t:array/0"><code>array:array()</code></a>|
    end

    test "app type", c do
      assert autolink_edoc("{@link //stdlib/array:array()}", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/array.html#t:array/0"><code>array:array()</code></a>|
    end

    @myList (if :erlang.system_info(:otp_release) >= ~c"27" do
               "t:myList/0"
             else
               "type-myList"
             end)

    test "abstract types - description", c do
      assert autolink_edoc("{@type myList(X). A special kind of lists ...}", c,
               extra_foo_code: "-export_type([myList/0]).\n-type myList() :: term().\n%% A type"
             ) ==
               ~s|<code><a href=\"##{@myList}\">myList</a>(X)</code>|
    end

    test "abstract types - description+dot", c do
      assert autolink_edoc("{@type myList(X, Y).}", c,
               extra_foo_code: "-export_type([myList/0]).\n-type myList() :: term().\n%% A type"
             ) ==
               ~s|<code><a href=\"##{@myList}\">myList</a>(X, Y)</code>|
    end

    test "abstract types - no description", c do
      assert autolink_edoc("{@type myList()}", c,
               extra_foo_code: "-export_type([myList/0]).\n-type myList() :: term().\n%% A type"
             ) ==
               ~s|<code><a href=\"##{@myList}\">myList()</a></code>|
    end
  end

  describe "autolink_doc/2 for edoc warnings" do
    @describetag warnings: :send

    test "bad module", c do
      assert warn(fn ->
               assert autolink_edoc("{@link bad}", c) == ~s|<code>bad</code>|
             end) =~ ~s|references module "bad" but it is undefined|
    end

    test "bad local function", c do
      assert warn(fn ->
               assert autolink_edoc("{@link bad/0}", c) == ~s|<code>bad/0</code>|
             end) =~ ~s|references function "bad/0" but it is undefined or private|
    end

    test "bad remote function", c do
      assert warn(fn ->
               assert autolink_edoc("{@link bad:bad/0}", c) == ~s|<code>bad:bad/0</code>|
             end) =~ ~s|references function "bad:bad/0" but it is undefined or private|
    end

    test "bad local type", c do
      assert warn(fn ->
               assert autolink_edoc("{@link bad()}", c) == ~s|<code>bad()</code>|
             end) =~ ~s|references type "t:bad/0" but it is undefined or private|
    end

    test "bad remote type", c do
      assert warn(fn ->
               assert autolink_edoc("{@link bad:bad()}", c) == ~s|<code>bad:bad()</code>|
             end) =~ ~s|references type "t:bad:bad/0" but it is undefined or private|
    end

    test "application", c do
      assert warn(fn ->
               assert autolink_edoc("{@link //foo}", c) == ~s|<code>//foo</code>|
             end) =~ ~s|invalid reference: foo:index|
    end

    test "filtered module", c do
      opts = [filtered_modules: [%ExDoc.ModuleNode{module: :lists, id: "lists"}]]

      assert warn(fn ->
               assert autolink_edoc("{@link lists}", c, opts) ==
                        ~s|<code>lists</code>|
             end) == "reference to a filtered module"
    end

    test "filtered module function", c do
      opts = [filtered_modules: [%ExDoc.ModuleNode{module: :lists, id: "lists"}]]

      assert warn(fn ->
               assert autolink_edoc("{@link lists:all/2}", c, opts) ==
                        ~s|<code>lists:all/2</code>|
             end) == "reference to a filtered module"
    end
  end

  describe "autolink_doc/2 for markdown" do
    test "module in module code", c do
      assert autolink_doc("`erlang_bar`", c) ==
               ~s|<code class="inline">erlang_bar</code>|
    end

    test "m:module in module code", c do
      assert autolink_doc("`m:erlang_bar`", c) ==
               ~s|<a href=\"erlang_bar.html\"><code class="inline">erlang_bar</code></a>|
    end

    test "m:module with anchor in module code", c do
      assert autolink_doc("`m:erlang_bar#anchor`", c) ==
               ~s|<a href=\"erlang_bar.html#anchor\"><code class="inline">erlang_bar</code></a>|
    end

    test "invalid m:module in module code", c do
      assert autolink_doc("`m:erlang_bar()`", c) ==
               ~s|<code class="inline">m:erlang_bar()</code>|
    end

    test "module in module code reference", c do
      assert autolink_doc("[`erlang_bar`](`erlang_bar`)", c) ==
               ~s|<a href=\"erlang_bar.html\"><code class="inline">erlang_bar</code></a>|
    end

    test "remote module with anchor in module code reference", c do
      assert autolink_doc("[`erlang_bar`](`erlang_bar#anchor`)", c) ==
               ~s|<a href=\"erlang_bar.html#anchor\"><code class="inline">erlang_bar</code></a>|

      assert autolink_doc("[`erlang_bar`](`m:erlang_bar#anchor`)", c) ==
               ~s|<a href=\"erlang_bar.html#anchor\"><code class="inline">erlang_bar</code></a>|
    end

    test "own module with anchor in module code reference", c do
      assert autolink_doc("[`erlang_foo`](`erlang_foo#anchor`)", c) ==
               ~s|<a href=\"erlang_foo.html#anchor\"><code class="inline">erlang_foo</code></a>|

      assert autolink_doc("[`erlang_foo`](`m:erlang_foo#anchor`)", c) ==
               ~s|<a href=\"erlang_foo.html#anchor\"><code class="inline">erlang_foo</code></a>|
    end

    test "function in module code", c do
      assert autolink_doc("`foo/0`", c) ==
               ~s|<a href=\"#foo/0\"><code class="inline">foo/0</code></a>|
    end

    test "function in module ref", c do
      assert autolink_doc("[`foo/0`](`foo/0`)", c) ==
               ~s|<a href="#foo/0"><code class="inline">foo/0</code></a>|
    end

    test "escaped function in module", c do
      assert autolink_doc(~S"`\c:ls/0`", c, deps: [stdlib: "stdlib/"]) ==
               ~s|<a href="stdlib/c.html#ls/0"><code class="inline">c:ls/0</code></a>|
    end

    ## There is a bug in EarmarkParser that leaves a trailing `)` in when parsing the
    ## markdown below. We expect the bug to be present right now so that we can update th
    ## testcase when it is fixed. See https://github.com/RobertDober/earmark_parser/issues/139
    ## for details about the bug.
    test "escaped function in module ref", c do
      assert autolink_doc(~S"[c](`\\c:ls/0`)", c, deps: [stdlib: "stdlib/"]) ==
               ~s|<a href="stdlib/c.html#ls/0">c</a>)|
    end

    test "function quoted", c do
      assert autolink_doc("`erlang_foo:'foo'/0`", c) ==
               ~s|<a href="#foo/0"><code class="inline">erlang_foo:'foo'/0</code></a>|
    end

    test "function quoted large", c do
      assert autolink_doc("`erlang_foo:'Foo'/0`", c,
               extra_foo_code: "-export(['Foo'/0]).\n'Foo'() -> ok.\n"
             ) ==
               ~s|<a href="#Foo/0"><code class="inline">erlang_foo:'Foo'/0</code></a>|
    end

    test "function unicode", c do
      assert autolink_doc("`erlang_foo:'😀'/0`", c,
               extra_foo_code: "-export(['😀'/0]).\n'😀'() -> ok.\n"
             ) ==
               ~s|<a href="#%F0%9F%98%80/0"><code class="inline">erlang_foo:'😀'/0</code></a>|
    end

    test "function in module autoimport", c do
      assert autolink_doc("`node()`", c) ==
               ~s|<code class="inline">node()</code>|
    end

    test "function in module autoimport using slash", c do
      assert autolink_doc("`node/0`", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/erlang.html#node/0"><code class="inline">node/0</code></a>|
    end

    test "type in module autoimport", c do
      assert autolink_doc("`t:integer()`", c) ==
               ~s|<code class="inline">t:integer()</code>|
    end

    test "type in module autoimport using slash", c do
      assert autolink_doc("`t:integer/0`", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/erlang.html#t:integer/0"><code class="inline">integer/0</code></a>|
    end

    test "bad function in module code", c do
      assert autolink_doc("`bad/0`", c) ==
               ~s|<code class="inline">bad/0</code>|
    end

    test "Elixir keyword function", c do
      assert autolink_doc("`do/0`", c, extra_foo_code: "-export([do/0]).\ndo() -> ok.\n") ==
               ~s|<a href="#do/0"><code class="inline">do/0</code></a>|
    end

    test "linking to auto-imported nil works", c do
      assert autolink_doc("[`[]`](`t:nil/0`)", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/erlang.html#t:nil/0"><code class="inline">[]</code></a>|
    end

    test "linking to local nil works", c do
      assert autolink_doc(
               "[`[]`](`t:nil/0`)",
               c,
               extra_foo_code: "-export_type([nil/0]).\n-type nil() :: [].\n"
             ) ==
               ~s|<a href="#t:nil/0"><code class="inline">[]</code></a>|
    end

    test "linking to local nil function works", c do
      assert autolink_doc(
               "[`nil`](`nil/0`)",
               c,
               extra_foo_code: "-export([nil/0]).\nnil() -> [].\n"
             ) ==
               ~s|<a href="#nil/0"><code class="inline">nil</code></a>|
    end

    test "linking to exported nil function works", c do
      assert autolink_doc(
               "[`nil`](`erlang_bar:nil/0`)",
               c,
               extra_bar_code: "-export([nil/0]).\nnil() -> [].\n"
             ) ==
               ~s|<a href="erlang_bar.html#nil/0"><code class="inline">nil</code></a>|
    end

    test "linking to local is_integer function works", c do
      assert autolink_doc(
               "[`is_integer`](`is_integer/1`)",
               c,
               extra_foo_code:
                 "-export([is_integer/1]).\nis_integer(I) -> erlang:is_integer(I).\n"
             ) ==
               ~s|<a href="#is_integer/1"><code class="inline">is_integer</code></a>|
    end

    test "linking to extra works", c do
      assert autolink_doc(
               "[extra](`e:foolib:extra.md`)",
               c
             ) ==
               ~s|<a href="https://foolib.com/extra.html">extra</a>|
    end

    test "linking to extra anchor works", c do
      assert autolink_doc(
               "[extra](`e:foolib:extra.md#anchor`)",
               c
             ) ==
               ~s|<a href="https://foolib.com/extra.html#anchor">extra</a>|
    end

    test "linking to extra xhtml works", c do
      assert autolink_doc(
               "[extra](`e:foolib:extra.xhtml`)",
               c
             ) ==
               ~s|<a href="https://foolib.com/extra.xhtml">extra</a>|
    end

    test "linking to extra otp docs works", c do
      assert autolink_doc(
               "[Unicode usage](`e:stdlib:unicode_usage.md`)",
               c
             ) ==
               ~s|<a href="https://www.erlang.org/doc/apps/stdlib/unicode_usage.html">Unicode usage</a>|
    end

    test "linking to extra umbrella docs works", c do
      assert autolink_doc(
               "[Unicode usage](`e:stdlib:unicode_usage.md`)",
               c,
               apps: [:stdlib]
             ) ==
               ~s|<a href="stdlib/unicode_usage.html">Unicode usage</a>|
    end

    test "anchor", c do
      assert autolink_doc("[Foo](#baz)", c) ==
               ~s|<a href="#baz">Foo</a>|
    end
  end

  describe "autolink_doc/2 for markdown warnings" do
    @describetag warnings: :send

    test "bad function in module", c do
      assert warn(
               fn ->
                 assert autolink_doc("\n`erlang_bar:bad/0`", c) ==
                          ~s|<code class="inline">erlang_bar:bad/0</code>|
               end,
               line: 2
             ) =~
               ~s|documentation references function "erlang_bar:bad/0" but it is undefined or private|
    end

    test "bad type in module", c do
      assert warn(
               fn ->
                 assert autolink_doc("\n`t:erlang_bar:bad/0`", c) ==
                          ~s|<code class="inline">t:erlang_bar:bad/0</code>|
               end,
               line: 2
             ) =~
               ~s|documentation references type "t:erlang_bar:bad/0" but it is undefined or private|
    end

    test "bad callback in module", c do
      assert warn(
               fn ->
                 assert autolink_doc("\n`c:erlang_bar:bad/0`", c) ==
                          ~s|<code class="inline">c:erlang_bar:bad/0</code>|
               end,
               line: 2
             ) =~ ~s|documentation references callback "c:erlang_bar:bad/0" but it is undefined|
    end

    test "bad local type in module", c do
      assert warn(
               fn ->
                 assert autolink_doc("\n`t:bad/0`", c) == ~s|<code class="inline">t:bad/0</code>|
               end,
               line: 2
             ) =~ ~s|documentation references type "t:bad/0" but it is undefined or private|
    end

    test "bad local callback in module", c do
      assert warn(
               fn ->
                 assert autolink_doc("\n`c:bad/0`", c) == ~s|<code class="inline">c:bad/0</code>|
               end,
               line: 2
             ) =~ ~s|documentation references callback "c:bad/0" but it is undefined|
    end

    test "bad function in module ref", c do
      assert warn(
               fn ->
                 assert autolink_doc("[Bad](`bad/0`)", c) == ~s|Bad|
               end,
               line: nil
             ) =~ ~s|documentation references function "bad/0" but it is undefined or private|
    end

    test "linking to local extra works does not work", c do
      assert warn(
               fn ->
                 assert autolink_doc("[extra](`e:extra.md`)", c) ==
                          ~s|extra|
               end,
               line: nil
             ) =~ ~r/documentation references "e:extra.md" but it is invalid/
    end

    test "linking to unknown application does not work", c do
      assert warn(
               fn ->
                 assert autolink_doc("[extra](`e:barlib:extra.md`)", c) ==
                          ~s|<a href=\"https://hexdocs.pm/barlib/extra.html\">extra</a>|
               end,
               line: nil
             ) =~
               ~r/documentation references "e:barlib:extra.md" but barlib cannot be found/
    end

    test "linking to unknown application with anchor does not work", c do
      assert warn(
               fn ->
                 assert autolink_doc("[extra](`e:barlib:extra.md#anchor`)", c) ==
                          ~s|<a href=\"https://hexdocs.pm/barlib/extra.html#anchor\">extra</a>|
               end,
               line: nil
             ) =~
               ~r/documentation references "e:barlib:extra.md#anchor" but barlib cannot be found/
    end

    test "filtered module", c do
      opts = [filtered_modules: [%ExDoc.ModuleNode{module: :lists, id: "lists"}]]

      assert warn(fn ->
               assert autolink_doc("`m:lists`", c, opts) ==
                        ~s|<code class="inline">m:lists</code>|
             end) =~ "reference to a filtered module"
    end

    test "filtered module callback", c do
      opts = [filtered_modules: [%ExDoc.ModuleNode{module: :gen_server, id: "gen_server"}]]

      assert warn(fn ->
               assert autolink_doc("`c:gen_server:handle_call/3`", c, opts) ==
                        ~s|<code class="inline">c:gen_server:handle_call/3</code>|
             end) =~ "reference to a filtered module"
    end
  end

  describe "autolink_doc/2 for extra" do
    test "function", c do
      assert autolink_extra("`erlang_foo:foo/0`", c) ==
               ~s|<a href="erlang_foo.html#foo/0"><code class="inline">erlang_foo:foo/0</code></a>|
    end

    test "OTP function", c do
      assert autolink_extra("`lists:reverse/1`", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/lists.html#reverse/1"><code class="inline">lists:reverse/1</code></a>|
    end

    test "type", c do
      assert autolink_extra("`t:erlang_bar:t/0`", c) ==
               ~s|<a href="erlang_bar.html#t:t/0"><code class="inline">erlang_bar:t/0</code></a>|
    end

    test "OTP type", c do
      assert autolink_extra("`t:array:array/0`", c) ==
               ~s|<a href="https://www.erlang.org/doc/man/array.html#t:array/0"><code class="inline">array:array/0</code></a>|
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
      assert autolink_extra("`bad:bad/0`", c) ==
               ~s|<code class="inline">bad:bad/0</code>|
    end

    test "bad function", c do
      assert autolink_extra("`bad/0`", c) ==
               ~s|<code class="inline">bad/0</code>|
    end

    test "invalid function", c do
      assert autolink_extra("`...a/0`", c) ==
               ~s|<code class="inline">...a/0</code>|
    end

    @tag warnings: :send
    test "bad type", c do
      assert warn(
               fn ->
                 assert autolink_extra("`t:bad:bad/0`", c) ==
                          ~s|<code class="inline">t:bad:bad/0</code>|
               end,
               file: "extra.md",
               line: 1
             ) =~ ~s|documentation references type "t:bad:bad/0" but it is undefined or private|
    end

    @tag warnings: :send
    test "bad type ref", c do
      assert warn(
               fn ->
                 assert autolink_extra("[t](`t:bad:bad/0`)", c) ==
                          ~s|t|
               end,
               file: "extra.md",
               line: nil
             ) =~ ~s|documentation references type "t:bad:bad/0" but it is undefined or private|
    end

    @tag warnings: :send
    test "bad callback", c do
      assert warn(
               fn ->
                 assert autolink_extra("`c:bad:bad/0`", c) ==
                          ~s|<code class="inline">c:bad:bad/0</code>|
               end,
               file: "extra.md",
               line: 1
             ) =~ ~s|documentation references callback "c:bad:bad/0" but it is undefined|
    end

    test "bad module", c do
      assert autolink_extra("`does_not_exist`", c) ==
               ~s|<code class="inline">does_not_exist</code>|
    end

    @tag warnings: :send
    test "bad module using m:", c do
      assert warn(
               fn ->
                 assert autolink_extra("`m:does_not_exist`", c) ==
                          ~s|<code class="inline">m:does_not_exist</code>|
               end,
               file: "extra.md",
               line: 1
             ) =~ ~r|documentation references module \"does_not_exist\" but it is undefined|
    end

    @tag warnings: :send
    test "bad module using m: and anchor", c do
      assert warn(
               fn ->
                 assert autolink_extra("`m:does_not_exist#anchor`", c) ==
                          ~s|<code class="inline">m:does_not_exist#anchor</code>|
               end,
               file: "extra.md",
               line: 1
             ) =~ ~r|documentation references module \"does_not_exist\" but it is undefined|
    end

    @opts [
      extras: %{"Foo Bar.md" => "foo-bar", "Bar Baz.livemd" => "bar-baz"}
    ]

    test "extras", c do
      assert autolink_doc("[Foo](Foo Bar.md)", c, @opts) ==
               ~s|<a href="foo-bar.html">Foo</a>|
    end

    test "extras livemd", c do
      assert autolink_doc("[Bar](Bar Baz.livemd)", c, @opts) ==
               ~s|<a href="bar-baz.html">Bar</a>|
    end

    test "extras xhtml", c do
      assert autolink_doc("[Foo](Foo Bar.md)", c, [ext: ".xhtml"] ++ @opts) ==
               ~s|<a href="foo-bar.xhtml">Foo</a>|
    end

    test "extras anchor", c do
      assert autolink_doc("[Foo](Foo Bar.md#baz)", c, @opts) ==
               ~s|<a href="foo-bar.html#baz">Foo</a>|
    end

    test "extras relative", c do
      assert autolink_doc("[Foo](../guide/Foo Bar.md)", c, @opts) ==
               ~s|<a href="foo-bar.html">Foo</a>|
    end
  end

  describe "autolink_spec/2" do
    test "spec", c do
      assert autolink_spec("-spec foo() -> t().", c) ==
               ~s|foo() -> <a href="#t:t/0">t</a>().|
    end

    test "spec when fun is called record", c do
      assert autolink_spec("-spec record(module()) -> [[{module(), atom()}]].", c) ==
               ~s|record(<a href="https://www.erlang.org/doc/man/erlang.html#t:module/0">module</a>())| <>
                 ~s| -> [[{<a href="https://www.erlang.org/doc/man/erlang.html#t:module/0">module</a>(),| <>
                 ~s| <a href="https://www.erlang.org/doc/man/erlang.html#t:atom/0">atom</a>()}]].|
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
      assert autolink_spec(
               ~S"-spec foo() -> #{atom() := sets:set(integer()), float() => t()}.",
               c
             ) ==
               ~S|foo() -> #{<a href="https://www.erlang.org/doc/man/erlang.html#t:atom/0">atom</a>() := <a href="https://www.erlang.org/doc/man/sets.html#t:set/1">sets:set</a>(<a href="https://www.erlang.org/doc/man/erlang.html#t:integer/0">integer</a>()), <a href="https://www.erlang.org/doc/man/erlang.html#t:float/0">float</a>() => <a href="#t:t/0">t</a>()}.|
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
               ~s[foo() -> #x{x :: <a href="https://www.erlang.org/doc/man/erlang.html#t:atom/0">atom</a>()} | <a href="#t:t/0">t</a>().]
    end

    test "record - two fields", c do
      assert autolink_spec(~s"-spec foo() -> #x{x :: atom(), y :: sets:set(integer())} | t().", c) ==
               ~s[foo() -> #x{x :: <a href="https://www.erlang.org/doc/man/erlang.html#t:atom/0">atom</a>(), y :: <a href="https://www.erlang.org/doc/man/sets.html#t:set/1">sets:set</a>(<a href="https://www.erlang.org/doc/man/erlang.html#t:integer/0">integer</a>())} | <a href="#t:t/0">t</a>().]
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
               ~s|foo() -> <a href="https://www.erlang.org/doc/man/sets.html#t:set/0">sets:set</a>().|
    end

    test "OTP private type", c do
      assert autolink_spec(~S"-spec foo() -> array:array_indx().", c) ==
               ~s|foo() -> <a href="https://www.erlang.org/doc/man/array.html#t:array_indx/0">array:array_indx</a>().|
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
      assert warn(fn ->
               assert autolink_spec(~S"-spec foo() -> bad:bad(atom()).", c, warnings: :send) ==
                        ~s|foo() -> bad:bad(<a href="https://www.erlang.org/doc/man/erlang.html#t:atom/0">atom</a>()).|
             end) =~ ~s|references type "bad:bad/1" but it is undefined or private|
    end
  end

  defp autolink_spec(binary, c, opts \\ []) when is_binary(binary) do
    fixtures(c, "")

    opts =
      opts
      |> Keyword.put_new(:current_module, :erlang_foo)
      |> Keyword.put_new(:current_kfa, {:function, :foo, 1})

    {:ok, tokens, _} = :erl_scan.string(String.to_charlist(binary))
    {:ok, ast} = :erl_parse.parse_form(tokens)
    ast = put_elem(ast, 1, :erl_anno.set_file(~c"test.erl", elem(ast, 1)))
    ExDoc.Language.Erlang.autolink_spec(ast, opts)
  end

  defp autolink_extra(text, c) do
    # Markdown is usually not valid EDoc
    fixtures(c, "")

    [{:p, _, [ast], _}] = ExDoc.Markdown.to_ast(text, [])

    opts = c |> Map.take([:warnings]) |> Enum.to_list()

    do_autolink_doc(
      ast,
      [current_module: nil, file: nil, module_id: nil, file: "extra.md"] ++ opts
    )
  end

  defp autolink_doc(text, c, opts \\ []) do
    # Markdown is usually not valid EDoc
    fixtures(c, "", opts)

    ast =
      case ExDoc.Markdown.to_ast(text, []) do
        [{:p, _, ast, _}] -> ast
        ast -> ast
      end

    opts = Keyword.merge(opts, c |> Map.take([:warnings]) |> Enum.to_list())

    do_autolink_doc(ast, opts)
  end

  defp autolink_edoc(doc, c, opts \\ []) do
    fixtures(c, doc, opts)

    {:docs_v1, _, _, "application/erlang+html", %{"en" => doc}, _, _} =
      Code.fetch_docs(:erlang_foo)

    opts = Keyword.merge(opts, c |> Map.take([:warnings]) |> Enum.to_list())

    html =
      doc
      |> ExDoc.DocAST.parse!("application/erlang+html")
      |> do_autolink_doc(opts)

    # OTP 27 wraps edoc in <p></p>
    html
    |> String.trim_leading("<p>")
    |> String.trim_trailing("</p>")
  end

  defp do_autolink_doc(doc, opts \\ []) do
    opts =
      opts
      |> Keyword.put(:language, ExDoc.Language.Erlang)
      |> Keyword.put_new(:warnings, :raise)
      |> Keyword.put_new(:current_module, :erlang_foo)
      |> Keyword.put_new(:file, "erlang_foo.erl")
      |> Keyword.put_new(:module_id, "erlang_foo")
      |> Keyword.put_new(:deps, foolib: "https://foolib.com")
      |> Keyword.drop([:extra_foo_code, :extra_bar_code])

    doc
    |> ExDoc.Language.Erlang.autolink_doc(opts)
    |> ExDoc.DocAST.to_string()
  end

  defp warn(fun, md \\ []) when is_function(fun, 0) do
    fun.()
    assert_received {:warn, message, metadata}

    if Keyword.has_key?(md, :line) do
      assert md[:line] == metadata[:line]
    end

    if Keyword.has_key?(md, :file) do
      assert md[:file] == metadata[:file]
    end

    message
  end

  defp fixtures(c, doc, opts \\ []) do
    :code.purge(:erlang_foo)
    :code.purge(:erlang_bar)

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
