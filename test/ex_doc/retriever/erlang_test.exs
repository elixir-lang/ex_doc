defmodule ExDoc.Retriever.ErlangTest do
  use ExUnit.Case, async: true
  alias ExDoc.{Retriever, DocAST, Language.Erlang}
  import TestHelper

  @moduletag :otp_eep48
  @moduletag :tmp_dir

  describe "docs_from_modules/2" do
    test "module", c do
      erlc(c, :mod, ~S"""
      %% @doc
      %% mod docs.
      -module(mod).
      -export([function1/0, function2/0]).

      %% @doc
      %% function1/0 docs.
      -spec function1() -> atom().
      function1() -> ok.

      %% @doc
      %% function2/0 docs.
      function2() -> ok.
      """)

      {[mod], []} = Retriever.docs_from_modules([:mod], %ExDoc.Config{})

      %ExDoc.ModuleNode{
        deprecated: nil,
        doc_line: _,
        docs: [function1, function2],
        docs_groups: [:Callbacks, :Functions],
        group: nil,
        id: "mod",
        language: ExDoc.Language.Erlang,
        module: :mod,
        nested_context: nil,
        nested_title: nil,
        rendered_doc: nil,
        source_path: _,
        source_url: nil,
        title: "mod",
        type: :module,
        typespecs: []
      } = mod

      assert DocAST.to_string(mod.doc) =~ "mod docs."

      %ExDoc.FunctionNode{
        annotations: [],
        arity: 0,
        defaults: [],
        deprecated: nil,
        doc_line: _,
        group: :Functions,
        id: "function1/0",
        name: :function1,
        rendered_doc: nil,
        signature: _,
        source_path: _,
        source_url: nil,
        specs: _,
        type: :function
      } = function1

      assert DocAST.to_string(function1.doc) =~ "function1/0 docs."
      assert Erlang.autolink_spec(hd(function1.specs), []) == "function1() -> atom()."

      %ExDoc.FunctionNode{
        id: "function2/0"
      } = function2

      assert DocAST.to_string(function2.doc) =~ "function2/0 docs."
      assert function2.specs == []
    end

    test "module with no docs is generated", c do
      erlc(c, :mod, ~S"""
      -module(mod).
      """)

      assert {[_], []} = Retriever.docs_from_modules([:mod], %ExDoc.Config{})
    end

    test "function with no docs is generated", c do
      erlc(c, :mod, ~S"""
      %% @doc Docs.
      -module(mod).
      -export([f/0]).

      f() -> ok.
      """)

      {[mod], []} = Retriever.docs_from_modules([:mod], %ExDoc.Config{})
      assert [_] = mod.docs
    end

    test "callbacks", c do
      erlc(c, :mod, ~S"""
      %% @doc Docs.
      -module(mod).

      -callback callback1() -> atom().
      %% callback1/0 docs.

      -callback optional_callback1() -> atom().
      %% optional_callback1/0 docs.

      -optional_callbacks([optional_callback1/0]).
      """)

      config = %ExDoc.Config{source_url_pattern: "%{path}:%{line}"}
      {[mod], []} = Retriever.docs_from_modules([:mod], config)
      [callback1, optional_callback1] = mod.docs

      assert callback1.id == "c:callback1/0"
      assert callback1.type == :callback
      assert callback1.annotations == []
      assert DocAST.to_string(callback1.doc) == "callback1/0 docs."
      assert Path.basename(callback1.source_url) == "mod.erl:4"
      assert Erlang.autolink_spec(hd(callback1.specs), []) == "callback1() -> atom()."

      assert optional_callback1.id == "c:optional_callback1/0"
      assert optional_callback1.type == :callback
      assert optional_callback1.annotations == ["optional"]
    end

    test "types", c do
      erlc(c, :mod, ~S"""
      %% @doc Docs.
      -module(mod).
      -export_type([type1/0, opaque1/0]).

      -type type1() :: atom().
      %% type1/0 docs.

      -opaque opaque1() :: atom().
      %% opaque1/0 docs.
      """)

      config = %ExDoc.Config{source_url_pattern: "%{path}:%{line}"}
      {[mod], []} = Retriever.docs_from_modules([:mod], config)
      [opaque1, type1] = mod.typespecs

      assert opaque1.id == "t:opaque1/0"
      assert opaque1.type == :opaque
      assert opaque1.signature == "opaque1/0"
      assert opaque1.doc |> DocAST.to_string() == "opaque1/0 docs."
      assert opaque1.spec |> Erlang.autolink_spec([]) == "opaque1()"

      assert type1.id == "t:type1/0"
      assert type1.type == :type
      assert type1.signature == "type1/0"
      assert type1.doc |> DocAST.to_string() == "type1/0 docs."
      assert type1.spec |> Erlang.autolink_spec([]) == "type1() :: atom()."
    end

    test "module with no chunk", c do
      erlc(
        c,
        :no_chunk,
        ~S"""
        -module(no_chunk).
        """,
        docs: false
      )

      assert {[], []} = Retriever.docs_from_modules([:no_chunk], %ExDoc.Config{})
    end

    # TODO
    # test "erts" do
    #   [mod] = Retriever.docs_from_modules([:erlang], %ExDoc.Config{})
    # end
  end
end
