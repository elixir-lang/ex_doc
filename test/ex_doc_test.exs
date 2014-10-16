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

  test "source_beam sets source dir" do
    options = [formatter: IdentityFormatter, retriever: IdentityRetriever,
               source_root: "root_dir", source_beam: "beam_dir"]
    {{source_dir, _retr_config}, _config} = ExDoc.generate_docs "Elixir", "1", options
    assert source_dir == options[:source_beam]
  end


  defp run(args) do
    ExDoc.CLI.run(args, &{&1, &2, &3})
  end

  test "minimum command-line options" do
    assert {"ExDoc", "1.2.3", [source_beam: "/"]} == run(["ExDoc", "1.2.3", "/"])
  end

  test "command-line config" do
    File.write!("test.config", ~s([key: "val"]))

    {project, version, opts} = run(["ExDoc", "--readme", "README.md", "1.2.3", "...", "-c", "test.config"])

    assert project == "ExDoc"
    assert version == "1.2.3"
    assert Enum.sort(opts) == [formatter_opts: [key: "val"], readme: "README.md", source_beam: "..."]
  after
    File.rm!("test.config")
  end
end
