Code.require_file "../test_helper", __FILE__

defmodule ExDocTest do
  use ExUnit::Case

  test "get_docs returns the documentation of a module" do
    file = File.expand_path("../fixtures/::Enum.beam", __FILE__)
    { :A, _ } = ExDoc.get_docs([file])
  end
end
