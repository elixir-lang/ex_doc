defmodule ExDoc.RetrieverTest do
  use ExUnit.Case, async: true
  alias ExDoc.{Retriever, DocAST}
  import TestHelper

  setup :create_tmp_dir

  describe "docs_from_modules/2: Elixir" do
    test "module", c do
      elixirc(c, ~S"""
      defmodule Mod do
        @moduledoc "Mod docs."

        @doc "function/0 docs."
        @spec function() :: atom()
        def function(), do: :ok

        def empty_doc_and_specs(), do: :ok

        @doc false
        def doc_false(), do: :ok
      end
      """)

      [mod] = Retriever.docs_from_modules([Mod], %ExDoc.Config{})

      assert %ExDoc.ModuleNode{
               doc_line: 2,
               id: "Mod",
               module: Mod,
               title: "Mod",
               type: :module,
               typespecs: [],
               docs: [empty_doc_and_specs, function]
             } = mod

      assert DocAST.to_string(mod.doc) == "<p>Mod docs.</p>"

      assert %ExDoc.FunctionNode{
               arity: 0,
               annotations: [],
               defaults: [],
               deprecated: nil,
               doc_line: 4,
               group: "Functions",
               id: "function/0",
               name: :function,
               rendered_doc: nil,
               signature: "function()",
               source_path: _,
               source_url: nil,
               specs: [spec],
               type: :function
             } = function

      assert DocAST.to_string(function.doc) == "<p>function/0 docs.</p>"

      assert %ExDoc.FunctionNode{
               id: "empty_doc_and_specs/0",
               doc: nil,
               specs: []
             } = empty_doc_and_specs

      assert Macro.to_string(spec) == "function() :: atom()"
    end

    test "Elixir functions with defaults", c do
      elixirc(c, ~S"""
      defmodule Mod do
        def foo(a, b \\ nil), do: {a, b}
      end
      """)

      [mod] = Retriever.docs_from_modules([Mod], %ExDoc.Config{})
      [foo] = mod.docs

      assert foo.id == "foo/2"
      assert foo.defaults == [foo: 1]
      assert foo.signature == "foo(a, b \\\\ nil)"
    end

    test "macros", c do
      elixirc(c, ~S"""
      defmodule Mod do
        @spec macro(Macro.t) :: Macro.t
        defmacro macro(quoted), do: quoted
      end
      """)

      [mod] = Retriever.docs_from_modules([Mod], %ExDoc.Config{})
      [macro] = mod.docs

      assert macro.id == "macro/1"
      assert macro.annotations == ["macro"]
      assert Macro.to_string(macro.specs) == "[macro(Macro.t()) :: Macro.t()]"
    end

    test "callbacks", c do
      elixirc(c, ~S"""
      defmodule Mod do
        @doc "callback1/0 docs."
        @callback callback1() :: :ok

        @callback optional_callback1() :: :ok

        @optional_callbacks optional_callback1: 0

        @macrocallback macrocallback1() :: :ok
      end
      """)

      config = %ExDoc.Config{source_url_pattern: "%{path}:%{line}"}
      [mod] = Retriever.docs_from_modules([Mod], config)
      assert mod.type == :behaviour

      [callback1, macrocallback1, optional_callback1] = mod.docs

      assert callback1.id == "callback1/0"
      assert callback1.signature == "callback1()"
      assert callback1.type == :callback
      assert callback1.annotations == []
      assert callback1.doc_line == 2
      assert Path.basename(callback1.source_url) == "nofile:3"
      assert DocAST.to_string(callback1.doc) == "<p>callback1/0 docs.</p>"
      assert Macro.to_string(callback1.specs) == "[callback1() :: :ok]"

      assert optional_callback1.id == "optional_callback1/0"
      assert optional_callback1.signature == "optional_callback1()"
      assert optional_callback1.type == :callback
      assert optional_callback1.annotations == ["optional"]
      assert optional_callback1.doc_line == 5
      assert Path.basename(optional_callback1.source_url) == "nofile:5"
      refute optional_callback1.doc
      assert Macro.to_string(optional_callback1.specs) == "[optional_callback1() :: :ok]"

      assert macrocallback1.id == "macrocallback1/0"
      assert macrocallback1.signature == "macrocallback1()"
      assert macrocallback1.type == :macrocallback
      assert macrocallback1.annotations == []
      assert macrocallback1.doc_line == 9
      assert Path.basename(macrocallback1.source_url) == "nofile:9"
      refute macrocallback1.doc
      assert Macro.to_string(macrocallback1.specs) == "[macrocallback1(term()) :: :ok]"

      elixirc(c, ~S"""
      defmodule Impl do
        @behaviour Mod

        def callback1(), do: :ok

        @doc "optional_callback1/0 docs."
        def optional_callback1(), do: :ok

        @doc false
        defmacro macrocallback1(), do: :ok
      end
      """)

      [impl] = Retriever.docs_from_modules([Impl], %ExDoc.Config{})
      [callback1, optional_callback1] = impl.docs

      assert callback1.id == "callback1/0"
      assert callback1.type == :function
      assert callback1.annotations == []

      assert callback1.doc ==
               ExDoc.Markdown.to_ast("Callback implementation for `c:Mod.callback1/0`.")

      assert optional_callback1.id == "optional_callback1/0"
      assert optional_callback1.type == :function
      assert DocAST.to_string(optional_callback1.doc) == "<p>optional_callback1/0 docs.</p>"
    end

    test "protocols", c do
      elixirc(c, ~S"""
      defprotocol Mod do
        def foo(thing)
      end

      defimpl Mod, for: Atom do
        def foo(thing), do: thing
      end
      """)

      [mod] = Retriever.docs_from_modules([Mod, Mod.Atom], %ExDoc.Config{})
      assert mod.type == :protocol

      [foo] = mod.docs
      assert foo.id == "foo/1"
    end

    test "structs", c do
      elixirc(c, ~S"""
      defmodule MyStruct do
        @doc "MyStruct docs."
        defstruct [:field]
      end
      """)

      [mod] = Retriever.docs_from_modules([MyStruct], %ExDoc.Config{})
      [my_struct] = mod.docs

      assert my_struct.id == "__struct__/0"
      assert my_struct.annotations == ["struct"]
      assert my_struct.signature == "%MyStruct{}"
    end

    test "exceptions", c do
      elixirc(c, ~S"""
      defmodule MyException do
        defexception [:message]
      end
      """)

      [mod] = Retriever.docs_from_modules([MyException], %ExDoc.Config{})
      assert mod.title == "MyException"
      assert mod.type == :exception

      # TODO: this is because `%ExDoc.Config{}.groups_for_modules == []`.
      #
      # We build the default groups (Exceptions, Deprecated) in lib/ex_doc.ex,
      # maybe we should do that in the retriever instead?
      #
      # Remember Exceptions is an Elixir specific thing so the default should
      # probably be language specific.
      refute mod.group
    end

    test "defdelegate", c do
      elixirc(c, ~S"""
      defmodule Mod do
        @doc "Doc override."
        defdelegate downcase(str), to: String

        defdelegate upcase(str), to: String
      end
      """)

      [mod] = Retriever.docs_from_modules([Mod], %ExDoc.Config{})
      [downcase, upcase] = mod.docs

      assert downcase.id == "downcase/1"
      assert downcase.signature == "downcase(str)"
      assert downcase.specs == []
      assert downcase.doc == ExDoc.Markdown.to_ast("Doc override.")

      assert upcase.id == "upcase/1"
      assert upcase.signature == "upcase(str)"
      assert upcase.specs == []
      assert upcase.doc == ExDoc.Markdown.to_ast("See `String.upcase/1`.")
    end

    test "Mix tasks", c do
      elixirc(c, ~S"""
      defmodule Mix.Tasks.MyTask do
        use Mix.Task

        @impl true
        def run(_), do: :ok
      end
      """)

      [mod] = Retriever.docs_from_modules([Mix.Tasks.MyTask], %ExDoc.Config{})
      assert mod.title == "mix my_task"
      assert mod.type == :task
      refute mod.group
    end

    test "Elixir special modules" do
      assert Retriever.docs_from_modules([:elixir_bootstrap, Elixir], %ExDoc.Config{}) == []
    end

    test "overlapping defaults", c do
      elixirc(c, ~S"""
      defmodule Mod do
        @doc "Basic example"
        def overlapping_defaults(one, two) when is_list(two),
          do: {one, two}

        @doc "Third default arg overrides previous def clause"
        def overlapping_defaults(one, two, three \\ []),
          do: {one, two, three}

        def two_defaults(one, two) when is_atom(one) and is_atom(two),
          do: {one, two}

        @doc "Two default args"
        def two_defaults(one, two, three \\ [], four \\ [])
            when is_list(one) and is_list(two) and is_list(three) and is_list(four),
            do: {one, two, three, four}

        def special_case(one, two) when is_atom(one) and is_atom(two),
          do: {one, two}

        @doc "This function defines an arity that is less than the one in the previous clause"
        def special_case(one, two \\ [], three \\ [], four \\ [])
            when is_list(one) and is_list(two) and is_list(three) and is_list(four),
            do: {one, two, three, four}

        defmacro in_the_middle(foo, bar) when is_list(foo) and is_list(bar),
          do: quote(do: {unquote(foo), unquote(bar)})

        @doc "default arg is in the middle"
        defmacro in_the_middle(foo, bar \\ Baz, baz),
          do: quote(do: {unquote(foo), unquote(bar), unquote(baz)})
      end
      """)

      [mod] = Retriever.docs_from_modules([Mod], %ExDoc.Config{})

      overlapping_defaults_2 = Enum.find(mod.docs, &(&1.id == "overlapping_defaults/2"))
      overlapping_defaults_3 = Enum.find(mod.docs, &(&1.id == "overlapping_defaults/3"))
      assert overlapping_defaults_2.defaults == []
      assert overlapping_defaults_3.defaults == []

      two_defaults_2 = Enum.find(mod.docs, &(&1.id == "two_defaults/2"))
      two_defaults_4 = Enum.find(mod.docs, &(&1.id == "two_defaults/4"))
      assert two_defaults_2.defaults == []
      assert two_defaults_4.defaults == [{:two_defaults, 3}]

      special_case_2 = Enum.find(mod.docs, &(&1.id == "special_case/2"))
      special_case_4 = Enum.find(mod.docs, &(&1.id == "special_case/4"))
      assert special_case_2.defaults == []
      assert special_case_4.defaults == [special_case: 1, special_case: 3]

      in_the_middle_2 = Enum.find(mod.docs, &(&1.id == "in_the_middle/2"))
      in_the_middle_3 = Enum.find(mod.docs, &(&1.id == "in_the_middle/3"))
      assert in_the_middle_2.defaults == []
      assert in_the_middle_3.defaults == []
    end
  end

  describe "docs_from_modules/2: Erlang" do
    @describetag :otp23

    test "module", c do
      erlc(c, :mod, ~S"""
      %% @doc
      %% mod docs.
      -module(mod).
      -export([function/0]).

      %% @doc
      %% function/0 docs.
      -spec function() -> atom().
      function() -> ok.
      """)

      edoc_to_chunk(:mod)
      [mod] = Retriever.docs_from_modules([:mod], %ExDoc.Config{})

      %ExDoc.ModuleNode{
        deprecated: nil,
        doc_line: _,
        docs: [function],
        function_groups: ["Functions"],
        group: nil,
        id: "mod",
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
        group: "Functions",
        id: "function/0",
        name: :function,
        rendered_doc: nil,
        # TODO: assert when edoc is fixed
        signature: _,
        source_path: _,
        source_url: nil,
        specs: _,
        type: :function
      } = function

      assert DocAST.to_string(function.doc) =~ "function/0 docs."
      assert Macro.to_string(function.specs) == "[function() :: atom()]"
    end

    @tag :otp24
    test "Erlang callbacks", c do
      erlc(c, :mod, ~S"""
      -module(mod).

      -callback callback1() -> atom().
      %% callback1/0 docs.
      """)

      edoc_to_chunk(:mod)
      config = %ExDoc.Config{source_url_pattern: "%{path}:%{line}"}
      [mod] = Retriever.docs_from_modules([:mod], config)
      [callback1] = mod.docs

      assert callback1.id == "callback1/0"
      assert callback1.type == :callback
      assert DocAST.to_string(callback1.doc) == "callback1/0 docs."
      assert Path.basename(callback1.source_url) == "mod.erl:3"
      # this is an edoc bug, it should be 4
      assert callback1.doc_line == 3
      assert Macro.to_string(callback1.specs) == "[callback1() :: atom()]"
    end

    @tag :otp24
    test "Erlang types", c do
      erlc(c, :mod, ~S"""
      -module(mod).
      -export_type([type1/0, opaque1/0]).

      -type type1() :: atom().
      %% type1/0 docs.

      -opaque opaque1() :: atom().
      %% opaque1/0 docs.
      """)

      edoc_to_chunk(:mod)
      config = %ExDoc.Config{source_url_pattern: "%{path}:%{line}"}
      [mod] = Retriever.docs_from_modules([:mod], config)
      [opaque1, type1] = mod.typespecs

      assert opaque1.id == "opaque1/0"
      assert opaque1.type == :opaque
      assert opaque1.signature == "opaque1/0"
      assert DocAST.to_string(opaque1.doc) == "opaque1/0 docs."
      assert Macro.to_string(opaque1.spec) == "opaque1()"

      assert type1.id == "type1/0"
      assert type1.type == :type
      assert type1.signature == "type1/0"
      assert DocAST.to_string(type1.doc) == "type1/0 docs."
      assert Macro.to_string(type1.spec) == "type1() :: atom()"
    end

    test "module with no chunk", c do
      erlc(c, :no_chunk, ~S"""
      -module(no_chunk).
      """)

      assert Retriever.docs_from_modules([:no_chunk], %ExDoc.Config{}) == []
    end
  end

  describe "docs_from_modules/2: Generic" do
    test "module with no docs", c do
      elixirc(c, ~S"""
      defmodule Mod do
      end
      """)

      [mod] = Retriever.docs_from_modules([Mod], %ExDoc.Config{})
      assert mod.doc == nil
    end

    test "metadata", c do
      elixirc(c, ~S"""
      defmodule Mod do
        @doc since: "1.0.0"
        @doc deprecated: "deprecation message"
        @doc foo: true
        def foo(), do: :ok
      end
      """)

      [mod] = Retriever.docs_from_modules([Mod], %ExDoc.Config{})
      [foo] = mod.docs
      assert foo.id == "foo/0"
      assert foo.annotations == ["since 1.0.0"]
      assert foo.deprecated == "deprecation message"
    end

    test "module groups", c do
      elixirc(c, ~S"""
      defmodule Foo do
      end

      defmodule Bar do
      end

      defmodule Baz do
      end

      defmodule Qux do
      end
      """)

      config = %ExDoc.Config{
        groups_for_modules: [
          "Group 1": [Foo, Bar],
          "Group 2": [Baz]
        ]
      }

      [qux, bar, foo, baz] = Retriever.docs_from_modules([Foo, Bar, Baz, Qux], config)
      assert %{module: Foo, group: :"Group 1"} = foo
      assert %{module: Bar, group: :"Group 1"} = bar
      assert %{module: Baz, group: :"Group 2"} = baz
      assert %{module: Qux, group: nil} = qux
    end

    test "function groups", c do
      elixirc(c, ~S"""
      defmodule Mod do
        @doc group: 1
        def foo(), do: :ok

        @doc group: 1
        def bar(), do: :ok

        @doc group: 2
        def baz(), do: :ok
      end
      """)

      config = %ExDoc.Config{
        groups_for_functions: [
          "Group 1": &(&1.group == 1),
          "Group 2": &(&1.group == 2)
        ]
      }

      [mod] = Retriever.docs_from_modules([Mod], config)
      [bar, baz, foo] = mod.docs

      assert %{id: "foo/0", group: "Group 1"} = foo
      assert %{id: "bar/0", group: "Group 1"} = bar
      assert %{id: "baz/0", group: "Group 2"} = baz
    end

    test "nesting", c do
      elixirc(c, ~S"""
      defmodule Nesting.Prefix.B.A do
      end

      defmodule Nesting.Prefix.B.B.A do
      end

      defmodule Nesting.Prefix.B.C do
      end

      defmodule Nesting.Prefix.C do
      end
      """)

      mods =
        Retriever.docs_from_modules(
          [Nesting.Prefix.B.A, Nesting.Prefix.B.C],
          %ExDoc.Config{nest_modules_by_prefix: ["Nesting.Prefix.B"]}
        )

      assert length(mods) == 2

      assert Enum.at(mods, 0).nested_context == "Nesting.Prefix.B"
      assert Enum.at(mods, 0).nested_title == "A"

      assert Enum.at(mods, 1).nested_context == "Nesting.Prefix.B"
      assert Enum.at(mods, 1).nested_title == "C"

      [mod] =
        Retriever.docs_from_modules([Nesting.Prefix.B.B.A], %ExDoc.Config{
          nest_modules_by_prefix: ["Nesting.Prefix.B.B.A"]
        })

      refute mod.nested_context
      refute mod.nested_title
    end

    test "fails when module is not available" do
      assert_raise Retriever.Error, "module NotAvailable is not defined/available", fn ->
        Retriever.docs_from_modules([NotAvailable], %ExDoc.Config{})
      end
    end

    test "source_url is relative to source_root", c do
      tmp_dir = Path.expand(c.tmp_dir)

      elixirc(c, "foo.ex", ~S"""
      defmodule Foo do
      end
      """)

      config = %ExDoc.Config{source_url_pattern: "%{path}:%{line}", source_root: nil}
      [mod] = Retriever.docs_from_modules([Foo], config)
      assert mod.source_url == "#{tmp_dir}/foo.ex:1"

      config = %ExDoc.Config{source_url_pattern: "%{path}:%{line}", source_root: tmp_dir}
      [mod] = Retriever.docs_from_modules([Foo], config)
      assert mod.source_url == "foo.ex:1"
    end
  end

  test "docs_from_dir/2: filter_prefix", c do
    elixirc(c, ~S"""
    defmodule A do
    end

    defmodule A.A do
    end

    defmodule B do
    end
    """)

    ebin_dir = Path.join(c.tmp_dir, "ebin")
    config = %ExDoc.Config{filter_prefix: "A"}
    [a, a_a] = Retriever.docs_from_dir(ebin_dir, config)

    assert a.id == "A"
    assert a_a.id == "A.A"
  end
end
