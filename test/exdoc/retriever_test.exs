Code.require_file "../../test_helper", __FILE__

defmodule ExDoc.RetrieverTest do
  use ExUnit.Case

  require ExDoc.Retriever, as: R

  test "get_docs returns the module name" do
    input_path = File.expand_path("test/tmp")
    file = "#{input_path}/__MAIN__/CompiledWithDocs.beam"

    assert_match [{ "CompiledWithDocs", _ }], R.get_docs([file], input_path)
  end

  test "get_docs returns the nested module name" do
    input_path = File.expand_path("test/tmp")
    file = "#{input_path}/__MAIN__/ExDocTest/Nested.beam"

    assert_match [{ "ExDocTest.Nested", _ }], R.get_docs([file], input_path)
  end

  test "get_docs returns the moduledoc info" do
    input_path = File.expand_path("test/tmp")
    file = "#{input_path}/__MAIN__/CompiledWithDocs.beam"

    [{ _, {moduledoc, _} }] = R.get_docs([file], input_path)

    assert_match { 1, "moduledoc\n\n\#\# Example\n    CompiledWithDocs.example\n" }, moduledoc
  end

  test "get_docs returns nil if there's no moduledoc info" do
    input_path = File.expand_path("test/tmp")
    file = "#{input_path}/__MAIN__/CompiledWithoutDocs.beam"

    [{ _, {moduledoc, _} }] = R.get_docs([file], input_path)

    assert_match { _, nil }, moduledoc
  end

  test "get_docs returns the doc info for each module function" do
    input_path = File.expand_path("test/tmp")
    file = "#{input_path}/__MAIN__/CompiledWithDocs.beam"

    [{ _, {_, doc} }] = R.get_docs([file], input_path)

    assert_match [{ {:example, 0}, 10, :def, "Some example"}, { {:"example_1", 0}, 13, :defmacro, "Another example"}], doc
  end

  test "get_docs returns an empty list if there's no docs info" do
    input_path = File.expand_path("test/tmp")
    file = "#{input_path}/__MAIN__/CompiledWithoutDocs.beam"

    [{ _, {_, doc} }] = R.get_docs([file], input_path)

    assert_empty doc
  end
end
