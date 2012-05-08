Code.require_file "../../test_helper", __FILE__

defmodule ExDoc.RetrieverTest do
  use ExUnit.Case

  require ExDoc.Retriever, as: R

  defp input_path do
    File.expand_path("test/tmp/__MAIN__")
  end

  defp get_docs(kind, names) do
    files = Enum.map names, fn(n) -> "#{input_path}/#{n}.beam" end
    Keyword.get R.get_docs(files, input_path), kind
  end

  ## MODULES

  test "get_docs returns the module id" do
    [node] = get_docs :modules, ["CompiledWithDocs"]
    assert node.id == "CompiledWithDocs"
  end

  test "get_docs returns the module" do
    [node] = get_docs :modules, ["CompiledWithDocs"]
    assert node.module == CompiledWithDocs
  end

  test "get_docs returns the nested module" do
    [node] = get_docs :modules, ["ExDocTest/Nested"]
    assert node.module == ExDocTest.Nested
  end

  test "get_docs returns the relative module name" do
    [node] = get_docs :modules, ["ExDocTest/Nested"]
    assert node.relative == "ExDocTest.Nested"
  end

  test "get_docs returns the moduledoc info" do
    [node] = get_docs :modules, ["CompiledWithDocs"]
    assert node.moduledoc == "moduledoc\n\n\#\# Example\n    CompiledWithDocs.example\n"
  end

  test "get_docs returns nil if there's no moduledoc info" do
    [node] = get_docs :modules, ["CompiledWithoutDocs"]
    assert node.moduledoc == nil
  end

  test "get_docs returns the doc info for each module function" do
    [node] = get_docs :modules, ["CompiledWithDocs"]
    [ example, example_1, example_without_docs ] = node.docs

    assert example.id == "example/0"
    assert example.doc == "Some example"
    assert example.type == :def

    assert example_1.id == "example_1/0"
    assert example_1.type == :defmacro

    assert example_without_docs.source == "https://github.com/elixir-lang/elixir/blob/master/test/fixtures/compiled_with_docs.ex\#L15"
    assert example_without_docs.doc == nil
  end

  test "get_docs returns an empty list if there's no docs info" do
    [node] = get_docs :modules, ["CompiledWithoutDocs"]
    assert node.docs == []
  end

  test "get_docs returns the source" do
    [node] = get_docs :modules, ["CompiledWithDocs"]
    assert node.source == "https://github.com/elixir-lang/elixir/blob/master/test/fixtures/compiled_with_docs.ex\#L1"
  end

  test "get_docs returns an empty list if there's no children" do
    [node] = get_docs :modules, ["CompiledWithDocs"]
    assert node.children == []
  end

  test "get_docs returns a list with children" do
    [node]  = get_docs :modules, ["CompiledWithDocs", "CompiledWithDocs/Nested"]
    [child] = node.children
    assert child.module   == CompiledWithDocs.Nested
    assert child.relative == "Nested"
  end

  ## RECORDS

  test "get_docs properly tag records" do
    [node] = get_docs :records, ["CompiledRecord"]
    assert node.type == :record
  end

  test "ignore records internal functions" do
    [node] = get_docs :records, ["CompiledRecord"]
    functions = Enum.map node.docs, fn(doc) -> doc.id end
    assert functions == [
      "bar/1","bar/2",
      "foo/1","foo/2",
      "new/0","new/1",
      "update_bar/2","update_foo/2"
    ]
  end

  ## EXCEPTIONS

  test "get_docs properly tag exceptions" do
    [node] = get_docs :records, ["RandomError"]
    assert node.type == :exception
  end

  test "ignore exceptions internal functions" do
    [node] = get_docs :records, ["RandomError"]
    functions = Enum.map node.docs, fn(doc) -> doc.id end
    assert functions == [
      "exception/1", "exception/2",
      "message/1","message/2",
      "new/0","new/1", "update_message/2"
    ]
  end

  ## PROTOCOLS

  test "get_docs properly tag protocols" do
    [node] = get_docs :protocols, ["CustomProtocol"]
    assert node.type == :protocol
  end

  test "ignore protocols internal functions" do
    [node] = get_docs :protocols, ["CustomProtocol"]
    functions = Enum.map node.docs, fn(doc) -> doc.id end
    assert functions == ["plus_one/1"]
  end

  ## IMPLEMENTATIONS

  test "get_docs properly tag implementations" do
    [node]  = get_docs :protocols, ["CustomProtocol/Number"]
    assert node.type == :impl
  end

  test "ignore impl internal functions" do
    [node]  = get_docs :protocols, ["CustomProtocol/Number"]
    functions = Enum.map node.docs, fn(doc) -> doc.id end
    assert functions == ["plus_one/1"]
  end
end
