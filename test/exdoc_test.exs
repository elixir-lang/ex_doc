Code.require_file "../test_helper", __FILE__

defmodule ExDocTest do
  use ExUnit::Case

  test "get_docs returns the module name" do
    file = File.expand_path("../fixtures/::Enum.beam", __FILE__)
    assert_match [{ "::Enum", _ }], ExDoc.get_docs([file])
  end
end
