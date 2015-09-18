Mix.start()
Mix.shell(Mix.Shell.Process)

defmodule Mix.Tasks.DocsTest do
  use ExUnit.Case, async: true

  def run(args, opts) do
    Mix.Tasks.Docs.run(args, opts, &{ &1, &2, &3 })
  end

  test "inflects values from app and version" do
    assert {"ex_doc", "0.1.0", [source_beam: _]} =
           run([], [app: :ex_doc, version: "0.1.0"])
  end

  test "uses the given name" do
    assert {"ExDoc", "0.1.0", [source_beam: _]} =
           run([], [app: :ex_doc, version: "0.1.0", name: "ExDoc"])
  end

  test "accepts modules in :main" do
    assert {"ex_doc", "dev", [source_beam: _, main: "Sample"]} =
           run([], [app: :ex_doc, docs: [main: Sample]])
  end

  test "accepts files in :main" do
    assert {"ex_doc", "dev", [source_beam: _, main: "another"]} =
           run([], [app: :ex_doc, docs: [main: "another"]])
  end

  test "accepts output in :output" do
    assert {"ex_doc", "dev", [source_beam: _, output: "hello"]} =
           run([], [app: :ex_doc, docs: [output: "hello"]])
  end

  test "parses output with higher preference than options" do
    assert {"ex_doc", "dev", [source_beam: _, output: "world"]} =
           run(~w(-o world), [app: :ex_doc, docs: [output: "hello"]])
  end

  test "accepts lazy docs" do
    assert {"ex_doc", "dev", [source_beam: _, main: "another"]} =
           run([], [app: :ex_doc, docs: fn -> [main: "another"] end])
  end

  test "accepts source_url from root" do
    assert {"ex_doc", "dev", [source_beam: _, source_url: "http://github.com/elixir-lang/ex_doc"]} =
           run([], [app: :ex_doc, source_url: "http://github.com/elixir-lang/ex_doc"])
  end
end
