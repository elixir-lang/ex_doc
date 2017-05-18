defmodule ExDoc.Formatter.JSONTest do
  use ExUnit.Case, async: true

  setup do
    File.rm_rf(output_dir())
    File.mkdir_p!(output_dir())
  end

  def output_dir() do
    Path.expand("../../tmp/json", __DIR__)
  end

  def beam_dir() do
    Path.expand("../../tmp/beam", __DIR__)
  end

  defp doc_config() do
    [project: "Elixir",
     version: "1.0.1",
     formatter: "json",
     output: output_dir(),
     source_root: beam_dir(),
     source_beam: beam_dir(),
     extras: ["test/fixtures/README.md"]]
  end

  defp doc_config(config) do
    Keyword.merge(doc_config(), config)
  end

  defp generate_docs(config) do
   ExDoc.generate_docs(config[:project], config[:version], config)
  end

  defp decode(content) do
    tree = %ExDoc.ProjectNode{
      items: %{
        modules: module_tree(),
        exceptions: module_tree(),
        protocols: module_tree(),
        tasks: module_tree(),
      }
    }
    Poison.decode!(content, as: tree)
  end

  defp module_tree() do
    [
      %ExDoc.LeanModuleNode{
        types: [%ExDoc.TypeNode{}],
        functions: [%ExDoc.FunctionNode{}],
        callbacks: [%ExDoc.FunctionNode{}],
      }
    ]
  end

  test "run generates JSON content" do
    generate_docs(doc_config([language: "fr"]))

    assert %ExDoc.ProjectNode{version: "1.0.1", name: "Elixir", language: "fr"} = output_dir() |> Path.join("elixir.json") |> File.read!() |> decode()
  end
end
