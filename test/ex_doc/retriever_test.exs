defmodule ExDoc.RetrieverTest do
  use ExUnit.Case, async: true

  alias ExDoc.Retriever

  defp docs_from_files(names, url_pattern \\ "http://example.com/%{path}#L%{line}") do
    files  = Enum.map names, fn(n) -> "test/tmp/Elixir.#{n}.beam" end
    config = %ExDoc.Config{source_url_pattern: url_pattern, source_root: File.cwd!}
    Retriever.docs_from_files(files, config)
  end

  ## MODULES

  test "docs_from_dir with filter prefix match docs_from_files" do
    config = %ExDoc.Config{filter_prefix: "CompiledWithDocs", source_root: File.cwd!}
    from_dir_nodes = Retriever.docs_from_dir("test/tmp/beam", config)
    file_nodes =
      ["Elixir.CompiledWithDocs.beam", "Elixir.CompiledWithDocs.Nested.beam"]
      |> Enum.map(&Path.join("test/tmp/beam", &1))
      |> Retriever.docs_from_files(config)

    assert from_dir_nodes == file_nodes
  end

  test "docs_from_files and docs_from_modules match" do
    config = %ExDoc.Config{source_url_pattern: "http://example.com/%{path}#L%{line}", source_root: File.cwd!}
    file_nodes = Retriever.docs_from_files(["test/tmp/Elixir.UndefParent.Undocumented.beam"], config)
    module_nodes = Retriever.docs_from_modules([UndefParent.Undocumented], config)
    assert file_nodes == module_nodes
  end

  test "docs_from_files returns the module id" do
    [module_node] = docs_from_files ["CompiledWithDocs"]
    assert module_node.id == "CompiledWithDocs"
  end

  test "docs_from_files returns the module title" do
    [module_node] = docs_from_files ["CompiledWithDocs"]
    assert module_node.title == "CompiledWithDocs"
  end

  test "docs_from_files returns the module" do
    [module_node] = docs_from_files ["CompiledWithDocs"]
    assert module_node.module == CompiledWithDocs
  end

  test "docs_from_files returns the moduledoc info" do
    [module_node] = docs_from_files ["CompiledWithDocs"]
    assert module_node.doc == "moduledoc\n\n\#\# Example ☃ Unicode > escaping\n    CompiledWithDocs.example\n\n### Example H3 heading\n\nexample\n"
  end

  test "docs_from_files returns nil if there's no moduledoc info" do
    [module_node] = docs_from_files ["CompiledWithoutDocs"]
    assert module_node.doc == nil
  end

  test "docs_from_files returns the doc info for each module function" do
    [module_node] = docs_from_files ["CompiledWithDocs"]
    [struct, example, example_1, _example_with_h3, example_without_docs] = module_node.docs

    assert struct.id == "__struct__/0"
    assert struct.doc == "Some struct"
    assert struct.type == :def
    assert struct.defaults == []
    assert struct.signature == "%CompiledWithDocs{}"

    assert example.id   == "example/2"
    assert example.doc  == "Some example"
    assert example.type == :def
    assert example.defaults == ["example/1"]
    assert example.signature == "example(foo, bar \\\\ Baz)"

    assert example_1.id   == "example_1/0"
    assert example_1.type == :defmacro
    assert example_1.defaults == []

    assert example_without_docs.source_url == "http://example.com/test/fixtures/compiled_with_docs.ex\#L29"
    assert example_without_docs.doc == nil
    assert example_without_docs.defaults == []
  end

  test "docs_from_files returns an empty list if there's no docs info" do
    [module_node] = docs_from_files ["CompiledWithoutDocs"]
    assert module_node.docs == []
  end

  test "docs_from_files returns the specs for each non-private function" do
    [module_node] = docs_from_files ["TypesAndSpecs"]
    [add, _, _] = module_node.docs

    assert add.id   == "add/2"
    assert add.doc  == nil
    assert add.type == :def
    assert Macro.to_string(add.specs) ==
           "[add(integer(), opaque()) :: integer()]"
  end

  test "docs_from_files returns the specs for non-private macros" do
    [module_node] = docs_from_files ["TypesAndSpecs"]
    [_, _, macro] = module_node.docs

    assert macro.id   == "macro_spec/1"
    assert macro.doc  == nil
    assert macro.type == :defmacro
    assert Macro.to_string(macro.specs) == "[macro_spec(term(), any()) :: {:ok, any()}]"
  end

  test "docs_from_files returns the spec info for each non-private module type" do
    [module_node] = docs_from_files ["TypesAndSpecs"]
    [opaque, public, ref] = module_node.typespecs

    assert opaque.name  == :opaque
    assert opaque.arity == 0
    assert opaque.id    == "opaque/0"
    assert opaque.type  == :opaque
    assert opaque.signature == "opaque()"
    assert Macro.to_string(opaque.spec) == "opaque()"

    assert public.name  == :public
    assert public.arity == 1
    assert public.id    == "public/1"
    assert public.type  == :type
    assert public.doc   == "A public type"
    assert public.signature == "public(t)"
    assert Macro.to_string(public.spec) ==
           "public(t) :: {t, String.t(), TypesAndSpecs.Sub.t(), opaque(), :ok | :error}"

    assert ref.name  == :ref
    assert ref.arity == 0
    assert ref.id    == "ref/0"
    assert ref.type  == :type
    assert ref.signature == "ref()"
    assert Macro.to_string(ref.spec) ==
           "ref() :: {:binary.part(), public(any())}"
  end

  test "docs_from_files returns the source" do
    [module_node] = docs_from_files ["CompiledWithDocs"], "http://foo.com/bar/%{path}#L%{line}"
    assert module_node.source_url == "http://foo.com/bar/test/fixtures/compiled_with_docs.ex\#L1"
  end

  test "docs_from_files returns the source when source_root set to nil" do
    files  = Enum.map ["CompiledWithDocs"], fn(n) -> "test/tmp/Elixir.#{n}.beam" end
    config = %ExDoc.Config{source_url_pattern: "%{path}:%{line}", source_root: nil}
    [module_node] = Retriever.docs_from_files(files, config)
    assert String.ends_with?(module_node.source_url, "/test/fixtures/compiled_with_docs.ex:1")
  end

  test "docs_from_modules fails when module is not available" do
    config = %ExDoc.Config{source_url_pattern: "http://example.com/%{path}#L%{line}", source_root: File.cwd!}
    assert_raise ExDoc.Retriever.Error, "module NotAvailable is not defined/available", fn ->
      docs_from_files(["NotAvailable"], config)
    end
  end

  test "specs order is preserved" do
    [module_node] = docs_from_files ["MultipleSpecs"]
    [function_node] = module_node.docs
    specs = Macro.to_string(function_node.specs)
    assert specs == "[range?(%Range{first: term(), last: term()}) :: true, range?(term()) :: false]"
  end

  if Version.match?(System.version(), ">=1.1.0") do
    test "callbacks with no docs included" do
      [module_node] = docs_from_files ["CallbacksNoDocs"]
      functions = Enum.map module_node.docs, fn(doc) -> doc.id end
      assert functions == ["connect/2", "id/1"]
    end
  end

  ## EXCEPTIONS

  test "docs_from_files properly tags exceptions" do
    [module_node] = docs_from_files ["RandomError"]
    assert module_node.type == :exception
  end

  ## TASKS

  test "docs_from_files properly tags tasks" do
    [module_node] = docs_from_files ["Mix.Tasks.TaskWithDocs"]
    assert module_node.type == :task
  end

  test "docs_from_files returns task id" do
    [module_node] = docs_from_files ["Mix.Tasks.TaskWithDocs"]
    assert module_node.id == "Mix.Tasks.TaskWithDocs"
  end

  test "docs_from_files returns task title" do
    [module_node] = docs_from_files ["Mix.Tasks.TaskWithDocs"]
    assert module_node.title == "task_with_docs"
  end

  ## BEHAVIOURS

  test "ignore behaviours internal functions" do
    [module_node] = docs_from_files ["CustomBehaviourOne"]
    functions = Enum.map module_node.docs, fn(doc) -> doc.id end
    assert functions == ["greet/1", "hello/1"]
    [greet, hello] = module_node.docs
    assert hello.type == :callback
    assert hello.signature == "hello(integer)"
    assert greet.type == :callback
    assert greet.signature == "greet(arg0)"
  end

  test "retrieves macro callbacks from behaviours" do
    [module_node] = docs_from_files ["CustomBehaviourTwo"]
    functions = Enum.map module_node.docs, fn(doc) -> doc.id end
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
    assert Enum.map(docs, &(&1.id)) == ["bye/1", "greet/1", "hello/1"]
    assert Enum.at(docs, 0).doc ==
      "A doc for this so it doesn't use 'Callback implementation for'"
    assert Enum.at(docs, 1).doc ==
      "Callback implementation for `c:CustomBehaviourOne.greet/1`."
    assert Enum.at(docs, 2).doc ==
      "This is a sample callback.\n\n\n" <>
      "Callback implementation for `c:CustomBehaviourOne.hello/1`."
  end

  ## PROTOCOLS

  test "docs_from_files properly tag protocols" do
    [module_node] = docs_from_files ["CustomProtocol"]
    assert module_node.type == :protocol
  end

  test "ignore protocols internal functions" do
    [module_node] = docs_from_files ["CustomProtocol"]
    functions = Enum.map module_node.docs, fn(doc) -> doc.id end
    assert functions == ["plus_one/1", "plus_two/1"]
  end

  ## IMPLEMENTATIONS

  test "docs_from_files properly tag implementations" do
    [module_node]  = docs_from_files ["CustomProtocol.Number"]
    assert module_node.type == :impl
  end

  test "ignore impl internal functions" do
    [module_node]  = docs_from_files ["CustomProtocol.Number"]
    functions = Enum.map module_node.docs, fn(doc) -> doc.id end
    assert functions == ["plus_one/1", "plus_two/1"]
  end
end
