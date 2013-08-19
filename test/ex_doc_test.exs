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

  test "specifying source_beam overrides source_root" do
    options = [formatter: IdentityFormatter, retriever: IdentityRetriever, 
               source_root: "root_dir", source_beam: "beam_dir"]
    {{source_dir, _retr_config}, _config} = ExDoc.generate_docs "Elixir", "1", options
    assert source_dir == options[:source_beam]
  end

  test "source_root gets /ebin appended to path" do
    options = [formatter: IdentityFormatter, retriever: IdentityRetriever, 
               source_root: "root_dir"]
    {{source_dir, _retr_config}, _config} = ExDoc.generate_docs "Elixir", "1", options
    assert source_dir == Path.join(options[:source_root], "ebin")
  end

  test "no source_root or source_beam defaults to File.cwd!/ebin" do
    options = [formatter: IdentityFormatter, retriever: IdentityRetriever]
    {{source_dir, _retr_config}, _config} = ExDoc.generate_docs "Elixir", "1", options
    assert source_dir == Path.join(File.cwd!, "ebin")
  end
end
