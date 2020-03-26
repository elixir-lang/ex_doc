defmodule ExDocTest do
  use ExUnit.Case

  # Simple retriever that returns whatever is passed into it
  defmodule IdentityRetriever do
    def docs_from_dir(source, config) do
      {source, config}
    end
  end

  # Simple formatter that returns whatever is passed into it
  defmodule IdentityFormatter do
    def run(modules, config) do
      {modules, config}
    end
  end

  test "build_config & normalize_options" do
    project = "Elixir"
    version = "1"

    opts_with_output =
      &[
        app: :test_app,
        formatter: IdentityFormatter,
        retriever: IdentityRetriever,
        source_root: "root_dir",
        source_beam: "beam_dir",
        output: &1
      ]

    {_, config} = ExDoc.generate_docs(project, version, opts_with_output.("test/tmp/ex_doc"))
    assert config.output == "test/tmp/ex_doc"

    {_, config} = ExDoc.generate_docs(project, version, opts_with_output.("test/tmp/ex_doc/"))
    assert config.output == "test/tmp/ex_doc"

    {_, config} = ExDoc.generate_docs(project, version, opts_with_output.("test/tmp/ex_doc//"))
    assert config.output == "test/tmp/ex_doc"
  end

  test "uses custom markdown processor" do
    project = "Elixir"
    version = "1"

    options = [
      app: :test_app,
      formatter: IdentityFormatter,
      markdown_processor: Sample,
      output: "test/tmp/ex_doc",
      retriever: IdentityRetriever,
      source_root: "root_dir",
      source_beam: "beam_dir"
    ]

    ExDoc.generate_docs(project, version, options)
    assert Application.fetch_env!(:ex_doc, :markdown_processor) == Sample
  after
    Application.delete_env(:ex_doc, :markdown_processor)
  end

  test "source_beam sets source dir" do
    options = [
      app: :test_app,
      formatter: IdentityFormatter,
      retriever: IdentityRetriever,
      source_root: "root_dir",
      source_beam: "beam_dir"
    ]

    {{source_dir, _retr_config}, _config} = ExDoc.generate_docs("Elixir", "1", options)
    assert source_dir == options[:source_beam]
  end

  test "formatter module not found" do
    project = "Elixir"
    version = "1"

    options = [
      app: :test_app,
      formatter: "pdf",
      retriever: IdentityRetriever,
      source_root: "root_dir",
      source_beam: "beam_dir"
    ]

    assert_raise RuntimeError,
                 "formatter module \"pdf\" not found",
                 fn -> ExDoc.generate_docs(project, version, options) end
  end

  test "version" do
    assert {:ok, _version} = Version.parse(ExDoc.version())
  end
end
