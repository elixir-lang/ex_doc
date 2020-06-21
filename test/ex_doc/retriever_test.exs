defmodule ExDoc.RetrieverTest do
  use ExUnit.Case, async: true

  alias ExDoc.Retriever

  defp docs_from_files(names, config \\ []) do
    files = Enum.map(names, fn n -> "test/tmp/Elixir.#{n}.beam" end)

    config =
      ExDoc.build_config(
        "foobar",
        "0.1",
        Keyword.merge(
          [
            app: :test_app,
            source_url_pattern: "http://example.com/%{path}#L%{line}",
            source_root: File.cwd!()
          ],
          config
        )
      )

    Retriever.docs_from_files(files, config)
  end

  describe "docs_from_dir" do
    test "matches files with filter prefix" do
      config = %ExDoc.Config{filter_prefix: "CompiledWithDocs", source_root: File.cwd!()}
      from_dir_nodes = Retriever.docs_from_dir("test/tmp/beam", config)

      file_nodes =
        ["Elixir.CompiledWithDocs.beam", "Elixir.CompiledWithDocs.Nested.beam"]
        |> Enum.map(&Path.join("test/tmp/beam", &1))
        |> Retriever.docs_from_files(config)

      assert from_dir_nodes == file_nodes
    end
  end

  describe "modules" do
    test "returns module nodes" do
      [module_node] = docs_from_files(["CompiledWithDocs"])
      assert module_node.id == "CompiledWithDocs"
      assert module_node.title == "CompiledWithDocs"
      assert module_node.module == CompiledWithDocs
      refute module_node.group
      refute module_node.nested_context
      refute module_node.nested_title
      assert module_node.source_url == "http://example.com/test/fixtures/compiled_with_docs.ex#L1"

      assert module_node.doc ==
               [
                 {:p, %{}, [], ["moduledoc"]},
                 {:h2, %{}, [], ["Example ☃ Unicode > escaping"]},
                 {:pre, %{}, [], [{:code, %{}, [], ["CompiledWithDocs.example"]}]},
                 {:h3, %{}, [], ["Example H3 heading"]},
                 {:p, %{}, [], ["example"]}
               ]
    end

    test "returns module group" do
      [module_node] =
        docs_from_files(["CompiledWithDocs"], groups_for_modules: [Group: [CompiledWithDocs]])

      assert module_node.group == :Group

      [module_node] =
        docs_from_files(["CompiledWithDocs"], groups_for_modules: [Group: ["CompiledWithDocs"]])

      assert module_node.group == :Group

      [module_node] =
        docs_from_files(["CompiledWithDocs"], groups_for_modules: [Group: ~r/^CompiledWith.?/])

      assert module_node.group == :Group
    end

    test "returns nesting information" do
      prefix = "Common.Nesting.Prefix.B"

      module_nodes =
        docs_from_files([prefix <> ".A", prefix <> ".C"], nest_modules_by_prefix: [prefix])

      assert length(module_nodes) > 0

      for module_node <- module_nodes do
        assert module_node.nested_context == prefix
        assert module_node.nested_title in ~w(A C)
      end

      name = "Common.Nesting.Prefix.B.B.A"
      [module_node] = docs_from_files([name], nest_modules_by_prefix: [name])

      refute module_node.nested_context
      refute module_node.nested_title
    end

    test "returns the function nodes for each module" do
      [module_node] =
        docs_from_files(["CompiledWithDocs"],
          groups_for_functions: [
            Example: &(&1[:purpose] == :example),
            Legacy: &is_binary(&1[:deprecated])
          ]
        )

      [struct, example, example_1, example_with_h3, example_without_docs, flatten, is_zero] =
        module_node.docs

      assert struct.id == "__struct__/0"
      assert struct.doc == [{:p, %{}, [], ["Some struct"]}]
      assert struct.type == :function
      assert struct.defaults == []
      assert struct.signature == "%CompiledWithDocs{}"
      assert struct.group == "Functions"

      assert example.id == "example/2"
      assert example.doc == [{:p, %{}, [], ["Some example"]}]
      assert example.type == :function
      assert example.defaults == ["example/1"]
      assert example.signature == "example(foo, bar \\\\ Baz)"
      assert example.deprecated == "Use something else instead"
      assert example.group == "Example"

      assert example_1.id == "example_1/0"

      assert example_1.doc == [
               {:p, %{}, [], ["Another example with &mdash; & &ndash; (— & –)"]}
             ]

      assert example_1.type == :macro
      assert example_1.defaults == []
      assert example_1.annotations == ["macro", "since 1.3.0"]

      assert example_with_h3.id == "example_with_h3/0"
      assert example_with_h3.group == "Example"

      assert example_without_docs.id == "example_without_docs/0"
      assert example_without_docs.doc == nil
      assert example_without_docs.defaults == []
      assert example_without_docs.group == "Legacy"

      assert example_without_docs.source_url ==
               "http://example.com/test/fixtures/compiled_with_docs.ex\#L37"

      assert flatten.id == "flatten/1"
      assert flatten.type == :function

      if Version.match?(System.version(), ">= 1.8.0") do
        assert flatten.doc == [
                 {:p, %{}, [],
                  [
                    "See ",
                    {:code, %{}, [], ["List.flatten/1"]},
                    "."
                  ]}
               ]
      end

      assert is_zero.id == "is_zero/1"
      assert is_zero.doc == [{:p, %{}, [], ["A simple guard"]}]
      assert is_zero.type == :macro
      assert is_zero.defaults == []
    end

    test "returns the specs for each non-private function" do
      [module_node] = docs_from_files(["TypesAndSpecs"])
      [add, _, _] = module_node.docs

      assert add.id == "add/2"
      assert add.doc == nil
      assert add.type == :function
      assert Macro.to_string(add.specs) == "[add(integer(), opaque()) :: integer()]"
    end

    test "returns the specs for non-private macros" do
      [module_node] = docs_from_files(["TypesAndSpecs"])
      [_, _, macro] = module_node.docs

      assert macro.id == "macro_spec/1"
      assert macro.doc == nil
      assert macro.type == :macro
      assert Macro.to_string(macro.specs) == "[macro_spec(any()) :: {:ok, any()}]"
    end

    test "returns the spec info for each non-private module type" do
      [module_node] = docs_from_files(["TypesAndSpecs"])
      [opaque, public] = module_node.typespecs

      assert opaque.name == :opaque
      assert opaque.arity == 0
      assert opaque.id == "opaque/0"
      assert opaque.type == :opaque
      assert opaque.signature == "opaque()"
      assert Macro.to_string(opaque.spec) == "opaque()"

      assert public.name == :public
      assert public.arity == 1
      assert public.id == "public/1"
      assert public.type == :type
      assert public.doc == [{:p, %{}, [], ["A public type"]}]
      assert public.signature == "public(t)"

      assert Macro.to_string(public.spec) ==
               "public(t) :: {t, String.t(), TypesAndSpecs.Sub.t(), opaque(), :ok | :error}"
    end

    test "returns the source when source_root set to nil" do
      files = Enum.map(["CompiledWithDocs"], fn n -> "test/tmp/Elixir.#{n}.beam" end)
      config = %ExDoc.Config{source_url_pattern: "%{path}:%{line}", source_root: nil}
      [module_node] = Retriever.docs_from_files(files, config)
      assert String.ends_with?(module_node.source_url, "/test/fixtures/compiled_with_docs.ex:1")
    end

    test "returns for modules without docs" do
      [module_node] = docs_from_files(["CompiledWithoutDocs"])
      assert module_node.doc == nil
      assert module_node.docs == []
    end

    test "returns callbacks with no docs included" do
      [module_node] = docs_from_files(["CallbacksNoDocs"])
      [connect, id] = module_node.docs

      assert connect.id == "connect/2"
      assert connect.annotations == []

      assert id.id == "id/1"
      assert id.deprecated == "Use another id"
      assert id.annotations == ["optional", "since 1.3.0"]
    end

    test "fails when module is not available" do
      assert_raise ExDoc.Retriever.Error, "module NotAvailable is not defined/available", fn ->
        docs_from_files(["NotAvailable"])
      end
    end
  end

  describe "exceptions" do
    test "are properly tagged" do
      [module_node] = docs_from_files(["RandomError"])
      assert module_node.type == :exception
      assert module_node.group == :Exceptions
    end
  end

  describe "deprecated" do
    test "are properly tagged" do
      [module_node] = docs_from_files(["Warnings"])
      assert module_node.group == :Deprecated
    end
  end

  describe "tasks" do
    test "are properly tagged" do
      [module_node] = docs_from_files(["Mix.Tasks.TaskWithDocs"])
      assert module_node.type == :task
      assert module_node.id == "Mix.Tasks.TaskWithDocs"
      assert module_node.title == "mix task_with_docs"
    end
  end

  ## BEHAVIOURS

  describe "behaviours" do
    test "returns callbacks (minus internal functions)" do
      [module_node] = docs_from_files(["CustomBehaviourOne"])
      functions = Enum.map(module_node.docs, fn doc -> doc.id end)
      assert functions == ["greet/1", "hello/1"]
      [greet, hello] = module_node.docs
      assert hello.type == :callback
      assert hello.signature == "hello(%URI{})"
      assert greet.type == :callback
      assert greet.signature == "greet(arg1)"
    end

    test "returns macro callbacks" do
      [module_node] = docs_from_files(["CustomBehaviourTwo"])
      functions = Enum.map(module_node.docs, fn doc -> doc.id end)
      assert functions == ["bye/1"]
      assert hd(module_node.docs).type == :macrocallback
      assert hd(module_node.docs).signature == "bye(integer)"
    end

    test "undocumented callback implementations get default doc" do
      [module_node] =
        ["CustomBehaviourOne", "CustomBehaviourTwo", "CustomBehaviourImpl"]
        |> docs_from_files()
        |> Enum.filter(&match?(%ExDoc.ModuleNode{id: "CustomBehaviourImpl"}, &1))

      docs = module_node.docs
      assert Enum.map(docs, & &1.id) == ["bye/1", "greet/1", "hello/1"]

      assert Enum.at(docs, 0).doc == [
               {:p, %{}, [],
                [
                  "Callback implementation for ",
                  {:code, %{}, [class: "inline"], ["c:CustomBehaviourTwo.bye/1"]},
                  "."
                ]}
             ]

      assert Enum.at(docs, 1).doc == [
               {:p, %{}, [], ["A doc so it doesn't use 'Callback implementation for'"]}
             ]

      assert Enum.at(docs, 2).doc == [
               {:p, %{}, [],
                [
                  "Callback implementation for ",
                  {:code, %{}, [class: "inline"], ["c:CustomBehaviourOne.hello/1"]},
                  "."
                ]}
             ]
    end
  end

  ## PROTOCOLS

  describe "protocols" do
    test "are properly tagged" do
      [module_node] = docs_from_files(["CustomProtocol"])
      assert module_node.type == :protocol
    end

    test "ignores internal functions" do
      [module_node] = docs_from_files(["CustomProtocol"])
      functions = Enum.map(module_node.docs, fn doc -> doc.id end)
      assert functions == ["plus_one/1", "plus_two/1"]
    end
  end

  describe "implementations" do
    test "are skipped" do
      assert [] = docs_from_files(["CustomProtocol.Number"])
    end
  end
end
