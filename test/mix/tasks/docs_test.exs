Mix.start()
Mix.shell(Mix.Shell.Process)

defmodule Mix.Tasks.DocsTest do
  use ExUnit.Case, async: true

  def run(args, opts) do
    Mix.Tasks.Docs.run(args, opts, &{ &1, &2, &3 })
  end

  test "inflects values from app and version" do
    assert run([], [app: :ex_doc, version: "0.1.0"]) ==
           { "ex_doc", "0.1.0", [main: "ExDoc"] }
  end

  test "uses the given name" do
    assert run([], [app: :ex_doc, version: "0.1.0", name: "ExDoc"]) ==
           { "ExDoc", "0.1.0", [main: "ExDoc"] }
  end

  test "accepts modules in :main" do
    assert run([], [app: :ex_doc, docs: [main: Sample]]) ==
           { "ex_doc", "dev", [main: "Sample"] }
  end

  test "accepts files in :main" do
    assert run([], [app: :ex_doc, docs: [main: "overview"]]) ==
           { "ex_doc", "dev", [main: "overview"] }
  end

  test "accepts output in :output" do
    assert run([], [app: :ex_doc, docs: [output: "hello"]]) ==
           { "ex_doc", "dev", [main: "ExDoc", output: "hello"] }
  end

  test "parses output with higher preference than options" do
    assert run(%w(-o world), [app: :ex_doc, docs: [output: "hello"]]) ==
           { "ex_doc", "dev", [main: "ExDoc", output: "world"] }
  end

  test "accepts lazy docs" do
    assert run([], [app: :ex_doc, docs: fn -> [main: "overview"] end]) ==
           { "ex_doc", "dev", [main: "overview"] }
  end

  test "accepts source_url from root" do
    assert run([], [app: :ex_doc, source_url: "http://github.com/elixir-lang/ex_doc"]) ==
           { "ex_doc", "dev", [main: "ExDoc", source_url: "http://github.com/elixir-lang/ex_doc"] }
  end
end
