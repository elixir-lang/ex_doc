Code.require_file "../../test_helper", __FILE__

defmodule ExDoc.RetrieverTest do
  use ExUnit.Case

  require ExDoc.Retriever, as: R

  defp input_path do
    File.expand_path("test/tmp")
  end

  test "get_docs returns the module id" do
    file = "#{input_path}/__MAIN__/CompiledWithDocs.beam"
    [node] = Keyword.get R.get_docs([file], input_path), :modules
    assert node.id == "CompiledWithDocs"
  end

  test "get_docs returns the module" do
    file = "#{input_path}/__MAIN__/CompiledWithDocs.beam"
    [node] = Keyword.get R.get_docs([file], input_path), :modules
    assert node.module == CompiledWithDocs
  end

  test "get_docs returns the nested module" do
    file = "#{input_path}/__MAIN__/ExDocTest/Nested.beam"
    [node] = Keyword.get R.get_docs([file], input_path), :modules
    assert node.module == ExDocTest.Nested
  end

  test "get_docs returns the relative module name" do
    file = "#{input_path}/__MAIN__/ExDocTest/Nested.beam"
    [node] = Keyword.get R.get_docs([file], input_path), :modules
    assert node.relative == "ExDocTest.Nested"
  end

  test "get_docs returns the moduledoc info" do
    file = "#{input_path}/__MAIN__/CompiledWithDocs.beam"
    [node] = Keyword.get R.get_docs([file], input_path), :modules
    assert node.moduledoc == "moduledoc\n\n\#\# Example\n    CompiledWithDocs.example\n"
  end

  test "get_docs returns nil if there's no moduledoc info" do
    file = "#{input_path}/__MAIN__/CompiledWithoutDocs.beam"
    [node] = Keyword.get R.get_docs([file], input_path), :modules
    assert node.moduledoc == nil
  end

  test "get_docs returns the doc info for each module function" do
    file = "#{input_path}/__MAIN__/CompiledWithDocs.beam"

    [node] = Keyword.get R.get_docs([file], input_path), :modules

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
    file = "#{input_path}/__MAIN__/CompiledWithoutDocs.beam"
    [node] = Keyword.get R.get_docs([file], input_path), :modules
    assert node.docs == []
  end

  test "get_docs returns the source" do
    file = "#{input_path}/__MAIN__/CompiledWithDocs.beam"
    [node] = Keyword.get R.get_docs([file], input_path), :modules
    assert node.source == "https://github.com/elixir-lang/elixir/blob/master/test/fixtures/compiled_with_docs.ex\#L1"
  end

  test "get_docs returns an empty list if there's no children" do
    file = "#{input_path}/__MAIN__/CompiledWithDocs.beam"
    [node] = Keyword.get R.get_docs([file], input_path), :modules
    assert node.children == []
  end

  test "get_docs returns a list with children" do
    files = [
      "#{input_path}/__MAIN__/CompiledWithDocs.beam",
      "#{input_path}/__MAIN__/CompiledWithDocs/Nested.beam"
    ]

    [node] = Keyword.get R.get_docs(files, input_path), :modules
    [child] = node.children
    assert child.module   == CompiledWithDocs.Nested
    assert child.relative == "Nested"
  end

end
