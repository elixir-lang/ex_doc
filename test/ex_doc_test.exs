defmodule ExDocTest do
  # We set markdown processor which is global
  use ExUnit.Case, async: false

  @moduletag :tmp_dir

  # Simple retriever that returns whatever is passed into it
  defmodule IdentityRetriever do
    def docs_from_dir(source, config) do
      {source, config}
    end
  end

  # Simple formatter that returns whatever is passed into it
  defmodule IdentityFormatter do
    def run(modules, _extras, config) do
      {modules, config}
    end
  end

  test "uses custom markdown processor", %{tmp_dir: tmp_dir} do
    project = "Elixir"
    version = "1"

    options = [
      apps: [:test_app],
      formatters: [IdentityFormatter],
      markdown_processor: Sample,
      output: tmp_dir <> "/ex_doc",
      retriever: IdentityRetriever
    ]

    ExDoc.generate(project, version, ["beam_dir"], options)
    assert Application.fetch_env!(:ex_doc, :markdown_processor) == {Sample, []}
  after
    Application.delete_env(:ex_doc, :markdown_processor)
  end

  test "uses custom markdown processor with custom options", %{tmp_dir: tmp_dir} do
    project = "Elixir"
    version = "1"

    options = [
      apps: [:test_app],
      formatters: [IdentityFormatter],
      markdown_processor: {Sample, [foo: :bar]},
      output: tmp_dir <> "/ex_doc",
      retriever: IdentityRetriever
    ]

    ExDoc.generate(project, version, ["beam_dir"], options)
    assert Application.fetch_env!(:ex_doc, :markdown_processor) == {Sample, [foo: :bar]}
  after
    Application.delete_env(:ex_doc, :markdown_processor)
  end

  test "source_beam sets source dir" do
    options = [
      apps: [:test_app],
      formatters: [IdentityFormatter],
      retriever: IdentityRetriever
    ]

    assert [%{entrypoint: {source_dirs, _config}}] =
             ExDoc.generate("Elixir", "1", ["beam_dir"], options)

    assert source_dirs == ["beam_dir"]
  end

  test "formatter module not found" do
    project = "Elixir"
    version = "1"
    options = [formatters: ["pdf"], retriever: IdentityRetriever]

    assert_raise RuntimeError,
                 "formatter module ExDoc.Formatter.PDF not found",
                 fn -> ExDoc.generate(project, version, ["beam_dir"], options) end
  end

  test "version" do
    assert {:ok, _version} = Version.parse(ExDoc.version())
  end
end
