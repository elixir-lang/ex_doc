Code.require_file "../../test_helper", __FILE__

defmodule ExDoc.RetrieverTest do
  use ExUnit.Case

  require ExDoc.Retriever, as: R

  defp input_path do
    File.expand_path("test/tmp")
  end

  test "get_docs returns the module id" do
    file = "#{input_path}/__MAIN__/CompiledWithDocs.beam"
    { [node], _, _ } = R.get_docs([file], input_path)
    assert node.id == "CompiledWithDocs"
  end

  test "get_docs returns the module" do
    file = "#{input_path}/__MAIN__/CompiledWithDocs.beam"
    { [node], _, _ } = R.get_docs([file], input_path)
    assert node.module == CompiledWithDocs
  end

  test "get_docs returns the nested module" do
    file = "#{input_path}/__MAIN__/ExDocTest/Nested.beam"
    { [node], _, _ } = R.get_docs([file], input_path)
    assert node.module == ExDocTest.Nested
  end

  test "get_docs returns the relative module name" do
    file = "#{input_path}/__MAIN__/ExDocTest/Nested.beam"
    { [node], _, _ } = R.get_docs([file], input_path)
    assert node.relative == "ExDocTest.Nested"
  end

  test "get_docs returns the moduledoc info" do
    file = "#{input_path}/__MAIN__/CompiledWithDocs.beam"
    { [node], _, _ } = R.get_docs([file], input_path)
    assert node.moduledoc == "moduledoc\n\n\#\# Example\n    CompiledWithDocs.example\n"
  end

  test "get_docs returns nil if there's no moduledoc info" do
    file = "#{input_path}/__MAIN__/CompiledWithoutDocs.beam"
    { [node], _, _ } = R.get_docs([file], input_path)
    assert node.moduledoc == nil
  end

  test "get_docs returns the doc info for each module function" do
    file = "#{input_path}/__MAIN__/CompiledWithDocs.beam"

    { [node], _, _ } = R.get_docs([file], input_path)

    assert node.docs == [
      {{:example, 0}, 10, :def, "Some example"},
      {{:"example_1", 0}, 13, :defmacro, "Another example"},
      {{:example_without_docs, 0}, 15, :def, nil}
    ]
  end

  test "get_docs returns an empty list if there's no docs info" do
    file = "#{input_path}/__MAIN__/CompiledWithoutDocs.beam"
    { [node], _, _ } = R.get_docs([file], input_path)
    assert node.docs == []
  end

  test "get_docs returns a relative source path" do
    file = "#{input_path}/__MAIN__/CompiledWithDocs.beam"
    { [node], _, _ } = R.get_docs([file], input_path)
    assert node.source == "test/fixtures/compiled_with_docs.ex"
  end

  test "get_docs returns an empty list if there's no children" do
    file = "#{input_path}/__MAIN__/CompiledWithDocs.beam"
    { [node], _, _ } = R.get_docs([file], input_path)
    assert node.children == []
  end

  test "get_docs returns a list with children" do
    files = [
      "#{input_path}/__MAIN__/CompiledWithDocs.beam",
      "#{input_path}/__MAIN__/CompiledWithDocs/Nested.beam"
    ]

    { [node], _, _ } = R.get_docs(files, input_path)
    [child] = node.children
    assert child.module   == CompiledWithDocs.Nested
    assert child.relative == "Nested"
  end

end
