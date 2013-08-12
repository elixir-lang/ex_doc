Code.require_file "../../test_helper.exs", __FILE__

defmodule ExDoc.RetrieverTest do
  use ExUnit.Case

  defp get_docs(names, url_pattern // "http://example.com/%{path}#L%{line}") do
    files  = Enum.map names, fn(n) -> "test/tmp/Elixir.#{n}.beam" end
    config = ExDoc.Config[source_url_pattern: url_pattern, source_root: File.cwd!]
    ExDoc.Retriever.docs_from_files(files, config)
  end

  ## MODULES

  test "get_docs returns the module id" do
    [node] = get_docs ["CompiledWithDocs"]
    assert node.id == "CompiledWithDocs"
  end

  test "get_docs returns the module" do
    [node] = get_docs ["CompiledWithDocs"]
    assert node.module == CompiledWithDocs
  end

  test "get_docs returns the nested module" do
    [node] = get_docs ["UndefParent.Nested"]
    assert node.module == UndefParent.Nested
  end

  test "get_docs returns the relative module name" do
    [node] = get_docs ["UndefParent.Nested"]
    assert node.relative == "UndefParent.Nested"
  end

  test "get_docs returns the moduledoc info" do
    [node] = get_docs ["CompiledWithDocs"]
    assert node.moduledoc == "moduledoc\n\n\#\# Example\n    CompiledWithDocs.example\n"
  end

  test "get_docs returns nil if there's no moduledoc info" do
    [node] = get_docs ["CompiledWithoutDocs"]
    assert node.moduledoc == nil
  end

  test "get_docs returns the doc info for each module function" do
    [node] = get_docs ["CompiledWithDocs"]
    [ example, example_1, example_without_docs ] = node.docs

    assert example.id   == "example/2"
    assert example.doc  == "Some example"
    assert example.type == :def
    assert [{ :foo, _, _ }, { ://, _, _ }] = example.signature

    assert example_1.id   == "example_1/0"
    assert example_1.type == :defmacro

    assert example_without_docs.source == "http://example.com/test/fixtures/compiled_with_docs.ex\#L15"
    assert example_without_docs.doc == nil
  end

  test "get_docs returns an empty list if there's no docs info" do
    [node] = get_docs ["CompiledWithoutDocs"]
    assert node.docs == []
  end

  test "get_docs returns the source" do
    [node] = get_docs ["CompiledWithDocs"], "http://foo.com/bar/%{path}#L%{line}"
    assert node.source == "http://foo.com/bar/test/fixtures/compiled_with_docs.ex\#L1"
  end

  # test "get_docs returns an empty list if there's no children" do
  #   [node] = get_docs ["CompiledWithDocs"]
  #   assert node.children == []
  # end

  # test "get_docs returns a list with children" do
  #   [node]  = get_docs ["CompiledWithDocs", "CompiledWithDocs.Nested"]
  #   [child] = node.children
  #   assert child.module   == CompiledWithDocs.Nested
  #   assert child.relative == "Nested"
  # end

  ## RECORDS

  test "get_docs properly tag records" do
    [node] = get_docs ["CompiledRecord"]
    assert node.type == :record
  end

  test "ignore records internal functions" do
    [node] = get_docs ["CompiledRecord"]
    functions = Enum.map node.docs, fn(doc) -> doc.id end
    assert functions == []
  end

  ## EXCEPTIONS

  test "get_docs properly tag exceptions" do
    [node] = get_docs ["RandomError"]
    assert node.type == :exception
  end

  test "ignore exceptions internal functions" do
    [node] = get_docs ["RandomError"]
    functions = Enum.map node.docs, fn(doc) -> doc.id end
    assert functions == []
  end

  ## BEHAVIOURS

  test "get_docs properly tag behaviours" do
    [node] = get_docs ["CustomBehaviour"]
    assert node.type == :behaviour
  end

  test "ignore behaviours internal functions" do
    [node] = get_docs ["CustomBehaviour"]
    functions = Enum.map node.docs, fn(doc) -> doc.id end
    assert functions == ["hello/1"]
    assert hd(node.docs).type == :defcallback
    assert hd(node.docs).signature == [{ :integer, [line: 7], [] }]
  end

  ## PROTOCOLS

  test "get_docs properly tag protocols" do
    [node] = get_docs ["CustomProtocol"]
    assert node.type == :protocol
  end

  test "ignore protocols internal functions" do
    [node] = get_docs ["CustomProtocol"]
    functions = Enum.map node.docs, fn(doc) -> doc.id end
    assert functions == ["plus_one/1", "plus_two/1"]
  end

  ## IMPLEMENTATIONS

  test "get_docs properly tag implementations" do
    [node]  = get_docs ["CustomProtocol.Number"]
    assert node.type == :impl
  end

  test "ignore impl internal functions" do
    [node]  = get_docs ["CustomProtocol.Number"]
    functions = Enum.map node.docs, fn(doc) -> doc.id end
    assert functions == ["plus_one/1", "plus_two/1"]
  end
end
