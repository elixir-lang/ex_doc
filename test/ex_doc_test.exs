defmodule ExDocTest do
  use ExUnit.Case, async: true

  test "formatter module not found" do
    project = "Elixir"
    version = "1"
    options = [formatters: ["pdf"]]

    assert_raise RuntimeError,
                 "formatter module ExDoc.Formatter.PDF not found",
                 fn -> ExDoc.generate(project, version, ["test/tmp/unknown"], options) end
  end

  test "version" do
    assert {:ok, _version} = Version.parse(ExDoc.version())
  end
end
