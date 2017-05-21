defmodule ExDocTest do
  use ExUnit.Case, async: true

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
    options = [formatter: IdentityFormatter, retriever: IdentityRetriever,
               source_root: "root_dir", source_beam: "beam_dir",]

    {_, config} = ExDoc.generate_docs project, version, Keyword.merge(options, [output: "test/tmp/ex_doc"])
    assert config.output == "test/tmp/ex_doc"

    {_, config} = ExDoc.generate_docs project, version, Keyword.merge(options, [output: "test/tmp/ex_doc/"])
    assert config.output == "test/tmp/ex_doc"

    {_, config} = ExDoc.generate_docs project, version, Keyword.merge(options, [output: "test/tmp/ex_doc//"])
    assert config.output == "test/tmp/ex_doc"
  end

  test "source_beam sets source dir" do
    options = [formatter: IdentityFormatter, retriever: IdentityRetriever,
               source_root: "root_dir", source_beam: "beam_dir"]
    {{source_dir, _retr_config}, _config} = ExDoc.generate_docs "Elixir", "1", options
    assert source_dir == options[:source_beam]
  end

  test "formatter module not found" do
    project = "Elixir"
    version = "1"
    options = [formatter: "pdf", retriever: IdentityRetriever,
               source_root: "root_dir", source_beam: "beam_dir"]

    assert_raise RuntimeError,
                 "Formatter module not found for: pdf",
                 fn -> ExDoc.generate_docs project, version, options end
  end

  test "default configuration" do
    assert Map.get(%ExDoc.Config{}, :formatter) == "html"
    assert ExDoc.Config.default(:formatter) == "html"

    assert Map.get(%ExDoc.Config{}, :language) == "en"
    assert ExDoc.Config.default(:language) == "en"

    assert Map.get(%ExDoc.Config{}, :output) == "./doc"
    assert ExDoc.Config.default(:output) == "./doc"

    assert Map.get(%ExDoc.Config{}, :retriever) == ExDoc.Retriever
    assert ExDoc.Config.default(:retriever) == ExDoc.Retriever

    assert Map.get(%ExDoc.Config{}, :source_ref) == "master"
    assert ExDoc.Config.default(:source_ref) == "master"
  end

  test "version" do
    assert ExDoc.version =~ ~r{\d+\.\d+\.\d+}
  end
end
