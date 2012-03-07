Code.require_file "../test_helper", __FILE__

defmodule ExDocTest do
  use ExUnit::Case

  test "get_docs returns the module name" do
    file = File.expand_path("../fixtures/::Enum.beam", __FILE__)
    assert_match [{ "::Enum", _ }], ExDoc.get_docs([file])
  end

  test "get_docs returns the moduledoc info" do
    file = File.expand_path("../fixtures/::Enum.beam", __FILE__)
    [{ _, {moduledoc, _} }] = ExDoc.get_docs([file])
    { _, doc_text } = moduledoc
    assert is_binary(doc_text)
  end

  test "get_docs returns nil if there's no moduledoc info" do
    file = File.expand_path("../fixtures/::ArgumentError.beam", __FILE__)
    [{ _, {moduledoc, _} }] = ExDoc.get_docs([file])
    { _, doc_text } = moduledoc
    assert_nil doc_text
  end
end
