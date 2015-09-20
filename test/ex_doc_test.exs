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

    {_, config} = ExDoc.generate_docs project, version, Keyword.merge(options, [output: "test/tmp/docs"])
    assert config.output == "test/tmp/docs"

    {_, config} = ExDoc.generate_docs project, version, Keyword.merge(options, [output: "test/tmp/docs/"])
    assert config.output == "test/tmp/docs"

    {_, config} = ExDoc.generate_docs project, version, Keyword.merge(options, [output: "test/tmp/docs//"])
    assert config.output == "test/tmp/docs"
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
               source_root: "root_dir", source_beam: "beam_dir",]

    assert_raise RuntimeError,
                 "Formatter module not found for: pdf",
                 fn -> ExDoc.generate_docs project, version, options end
  end

  test "version" do
    assert ExDoc.version =~ ~r{\d+\.\d+\.\d+} 
  end

  test "sort" do
    assert ExDoc.sort([
      %ExDoc.FunctionNode{arity: 2, id: "//2", name: :/, },
      %ExDoc.FunctionNode{arity: 2, id: "!=/2", name: :!=, },
      %ExDoc.FunctionNode{arity: 100, id: "zoom/100", name: :zoom, },
      %ExDoc.FunctionNode{arity: 1, id: "run/1", name: :run, },
      %ExDoc.FunctionNode{arity: 3, id: "run/3", name: :run, },
      %ExDoc.FunctionNode{arity: 2, id: "--/2", name: :--, },
      %ExDoc.FunctionNode{arity: 2, id: "run/2", name: :run, },
    ]) == [
      %ExDoc.FunctionNode{arity: 2, id: "!=/2", name: :!=, },
      %ExDoc.FunctionNode{arity: 2, id: "--/2", name: :--, },
      %ExDoc.FunctionNode{arity: 2, id: "//2", name: :/, },
      %ExDoc.FunctionNode{arity: 1, id: "run/1", name: :run, },
      %ExDoc.FunctionNode{arity: 2, id: "run/2", name: :run, },
      %ExDoc.FunctionNode{arity: 3, id: "run/3", name: :run, },
      %ExDoc.FunctionNode{arity: 100, id: "zoom/100", name: :zoom, },
    ]
  end

end
