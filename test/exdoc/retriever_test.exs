Code.require_file "../../test_helper", __FILE__

defmodule ExDoc::RetrieverTest do
  use ExUnit::Case

  require ExDoc::Retriever, as: R

  test "get_docs returns the module name" do
    file = File.expand_path("../../tmp/::CompiledWithDocs.beam", __FILE__)
    assert_match [{ "::CompiledWithDocs", _ }], R.get_docs([file])
  end

  test "get_docs returns the moduledoc info" do
    file = File.expand_path("../../tmp/::CompiledWithDocs.beam", __FILE__)
    [{ _, {moduledoc, _} }] = R.get_docs([file])
    assert_match { 1, "moduledoc" }, moduledoc
  end

  test "get_docs returns nil if there's no moduledoc info" do
    file = File.expand_path("../../tmp/::CompiledWithoutDocs.beam", __FILE__)
    [{ _, {moduledoc, _} }] = R.get_docs([file])
    assert_match { _, nil }, moduledoc
  end

  test "get_docs returns the doc info for each module function" do
    file = File.expand_path("../../tmp/::CompiledWithDocs.beam", __FILE__)
    [{ _, {_, doc} }] = R.get_docs([file])
    assert_match [{ {:example, 0}, 5, :def, "Some example"}, { {:"example_1", 0}, 8, :def, "Another example"}], doc
  end

  test "get_docs returns an empty list if there's no docs info" do
    file = File.expand_path("../../tmp/::CompiledWithoutDocs.beam", __FILE__)
    [{ _, {_, doc} }] = R.get_docs([file])
    assert_empty doc
  end
end
