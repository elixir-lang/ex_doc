defmodule ExDoc.RetrieverTest do
  use ExUnit.Case, async: true
  alias ExDoc.Retriever
  import TestHelper

  @moduletag :tmp_dir

  describe "docs_from_modules/2: Generic" do
    test "module with no docs", c do
      elixirc(c, ~S"""
      defmodule A do
      end
      """)

      {[mod], []} = Retriever.docs_from_modules([A], %ExDoc.Config{})
      assert mod.doc == nil
    end

    test "metadata", c do
      elixirc(c, ~S"""
      defmodule A do
        @doc since: "1.0.0"
        @doc deprecated: "deprecation message"
        @doc foo: true
        def foo(), do: :ok
      end
      """)

      {[mod], []} = Retriever.docs_from_modules([A], %ExDoc.Config{})
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
          {"Group 1", [Foo, Bar]},
          {"Group 2", [Baz]}
        ]
      }

      {[qux, bar, foo, baz], []} = Retriever.docs_from_modules([Foo, Bar, Baz, Qux], config)
      assert %{module: Foo, group: "Group 1"} = foo
      assert %{module: Bar, group: "Group 1"} = bar
      assert %{module: Baz, group: "Group 2"} = baz
      assert %{module: Qux, group: nil} = qux
    end

    test "function groups", c do
      elixirc(c, ~S"""
      defmodule A do
        @doc group: 1
        def foo(), do: :ok

        @doc group: 1
        def bar(), do: :ok

        @doc group: 2
        def baz(), do: :ok
      end
      """)

      config = %ExDoc.Config{
        groups_for_docs: [
          {"Group 1", &(&1.group == 1)},
          {"Group 2", &(&1.group == 2)}
        ]
      }

      {[mod], []} = Retriever.docs_from_modules([A], config)
      [bar, baz, foo] = mod.docs

      assert %{id: "foo/0", group: "Group 1"} = foo
      assert %{id: "bar/0", group: "Group 1"} = bar
      assert %{id: "baz/0", group: "Group 2"} = baz
    end

    test "function groups use :group metadata", c do
      elixirc(c, ~S"""
      defmodule A do
        @doc group: "a"
        @callback foo() :: :ok

        @doc group: "b"
        def bar(), do: :ok

        @doc group: "c"
        def baz(), do: :ok
      end
      """)

      config = %ExDoc.Config{}
      {[mod], []} = Retriever.docs_from_modules([A], config)
      [bar, baz, foo] = mod.docs

      assert %{id: "c:foo/0", group: "a"} = foo
      assert %{id: "bar/0", group: "b"} = bar
      assert %{id: "baz/0", group: "c"} = baz
    end

    test "function groups use default_group_for_doc", c do
      elixirc(c, ~S"""
      defmodule A do
        @doc semi_group: "a"
        @callback foo() :: :ok

        @doc semi_group: "b"
        def bar(), do: :ok

        @doc semi_group: "c"
        def baz(), do: :ok
      end
      """)

      config = %ExDoc.Config{default_group_for_doc: & &1[:semi_group]}
      {[mod], []} = Retriever.docs_from_modules([A], config)
      [bar, baz, foo] = mod.docs

      assert %{id: "c:foo/0", group: "a"} = foo
      assert %{id: "bar/0", group: "b"} = bar
      assert %{id: "baz/0", group: "c"} = baz
    end

    test "function annotations", c do
      elixirc(c, ~S"""
      defmodule A do
        def foo(), do: :ok
      end
      """)

      {[mod], []} =
        Retriever.docs_from_modules([A], %ExDoc.Config{
          annotations_for_docs: fn metadata ->
            [metadata[:module], metadata[:name], metadata[:arity], metadata[:kind]]
          end
        })

      [foo] = mod.docs
      assert foo.id == "foo/0"
      assert foo.annotations == [A, :foo, 0, :function]
    end

    test "function annotations override", c do
      elixirc(c, ~S"""
      defmodule A do
        @doc module: B
        @doc name: :bar
        @doc arity: 1
        @doc kind: :type
        def foo(), do: :ok
      end
      """)

      {[mod], []} =
        Retriever.docs_from_modules([A], %ExDoc.Config{
          annotations_for_docs: fn metadata ->
            [metadata[:module], metadata[:name], metadata[:arity], metadata[:kind]]
          end
        })

      [foo] = mod.docs
      assert foo.id == "foo/0"
      assert foo.annotations == [B, :bar, 1, :type]
    end

    test "custom function annotations", c do
      elixirc(c, ~S"""
      defmodule A do
        @doc since: "1.0.0"
        @doc deprecated: "deprecation message"
        @doc foo: true
        def foo(), do: :ok
      end
      """)

      {[mod], []} =
        Retriever.docs_from_modules([A], %ExDoc.Config{
          annotations_for_docs: fn metadata ->
            if metadata[:foo] do
              [:baz]
            else
              []
            end
          end
        })

      [foo] = mod.docs
      assert foo.id == "foo/0"
      assert foo.annotations == [:baz, "since 1.0.0"]
      assert foo.deprecated == "deprecation message"
    end

    test "custom callback annotations", c do
      elixirc(c, ~S"""
      defmodule A do
        @doc foo: true
        @callback callback_name() :: :ok
      end
      """)

      {[mod], []} =
        Retriever.docs_from_modules([A], %ExDoc.Config{
          annotations_for_docs: fn metadata ->
            if metadata[:foo] do
              [:baz]
            else
              []
            end
          end
        })

      [foo] = mod.docs

      assert foo.annotations == [:baz]
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

      {mods, []} =
        Retriever.docs_from_modules(
          [Nesting.Prefix.B.A, Nesting.Prefix.B.C],
          %ExDoc.Config{nest_modules_by_prefix: ["Nesting.Prefix.B"]}
        )

      assert length(mods) == 2

      assert Enum.at(mods, 0).nested_context == "Nesting.Prefix.B"
      assert Enum.at(mods, 0).nested_title == ".A"

      assert Enum.at(mods, 1).nested_context == "Nesting.Prefix.B"
      assert Enum.at(mods, 1).nested_title == ".C"

      {[mod], []} =
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
  end

  test "docs_from_dir/2: filter_module", c do
    elixirc(c, ~S"""
    defmodule A do
    end

    defmodule A.A do
    end

    defmodule B do
    end
    """)

    ebin_dir = Path.join(c.tmp_dir, "ebin")
    config = %ExDoc.Config{filter_modules: fn module, _ -> Atom.to_string(module) =~ "A" end}

    assert {
             [%{id: "A"}, %{id: "A.A"}],
             [%{id: "B"}]
           } = Retriever.docs_from_dir(ebin_dir, config)

    assert {
             [%{id: "A"}, %{id: "A.A"}],
             [%{id: "B"}]
           } = Retriever.docs_from_dir([ebin_dir], config)
  end

  test "natural sorting", c do
    elixirc(c, ~S"""
    defmodule NaturallySorted do
      def function_b(), do: :ok
      def function_B(), do: :ok
      def function_A(), do: :ok
      def function_a(), do: :ok
      def function_A(arg), do: arg
      def function_a(arg), do: arg
    end
    """)

    {[mod], []} = Retriever.docs_from_modules([NaturallySorted], %ExDoc.Config{})

    [function_A_0, function_A_1, function_a_0, function_a_1, function_B_0, function_b_0] =
      mod.docs

    assert function_A_0.id == "function_A/0"
    assert function_A_1.id == "function_A/1"
    assert function_a_0.id == "function_a/0"
    assert function_a_1.id == "function_a/1"
    assert function_B_0.id == "function_B/0"
    assert function_b_0.id == "function_b/0"
  end

  test "no whitespace in signature", c do
    elixirc(c, ~S"""
    defmodule NoWhitespaceInSignature do
      @callback callback_name(
        arg1 :: integer(),
        1,
        %Date{},
        term,
        String.t()
      ) :: :ok
    end
    """)

    {[module_node], []} = Retriever.docs_from_modules([NoWhitespaceInSignature], %ExDoc.Config{})
    %{docs: [%{signature: signature}]} = module_node
    assert signature == "callback_name(arg1, integer, %Date{}, term, t)"
  end
end
