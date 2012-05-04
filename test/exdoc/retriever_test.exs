Code.require_file "../../test_helper", __FILE__

defmodule ExDoc.RetrieverTest do
  use ExUnit.Case

  require ExDoc.Retriever, as: R

  defp input_path do
    File.expand_path("test/tmp")
  end

  test "get_docs returns the module name" do
    file = "#{input_path}/__MAIN__/CompiledWithDocs.beam"
    [node] = R.get_docs([file], input_path)
    assert node.name == "CompiledWithDocs"
  end

  test "get_docs returns the nested module name" do
    file = "#{input_path}/__MAIN__/ExDocTest/Nested.beam"
    [node] = R.get_docs([file], input_path)
    assert node.name == "ExDocTest.Nested"
  end

  test "get_docs returns the moduledoc info" do
    file = "#{input_path}/__MAIN__/CompiledWithDocs.beam"
    [node] = R.get_docs([file], input_path)
    assert node.moduledoc == { 1, "moduledoc\n\n\#\# Example\n    CompiledWithDocs.example\n" }
  end

  test "get_docs returns nil if there's no moduledoc info" do
    file = "#{input_path}/__MAIN__/CompiledWithoutDocs.beam"
    [node] = R.get_docs([file], input_path)
    assert node.moduledoc == { 1, nil }
  end

  test "get_docs returns the doc info for each module function" do
    file = "#{input_path}/__MAIN__/CompiledWithDocs.beam"

    [node] = R.get_docs([file], input_path)

    assert node.docs == [
      {{:example, 0}, 10, :def, "Some example"},
      {{:"example_1", 0}, 13, :defmacro, "Another example"},
      {{:example_without_docs, 0}, 15, :def, nil}
    ]
  end

  test "get_docs returns an empty list if there's no docs info" do
    file = "#{input_path}/__MAIN__/CompiledWithoutDocs.beam"
    [node] = R.get_docs([file], input_path)
    assert node.docs == []
  end

  test "get_docs returns a relative source path" do
    file = "#{input_path}/__MAIN__/CompiledWithDocs.beam"
    [node] = R.get_docs([file], input_path)
    assert node.source == "test/fixtures/compiled_with_docs.ex"
  end
end
