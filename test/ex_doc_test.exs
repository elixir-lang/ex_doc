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

    def create_assets(_), do: :ok
    def create_index(_, _), do: :ok

    def needs_output_dir, do: false
  end

  test "source_beam sets source dir" do
    options = [formatter: IdentityFormatter, retriever: IdentityRetriever,
               source_root: "root_dir", source_beam: "beam_dir",
               create_assets: false, create_index: false]
    {{source_dir, _retr_config}, _config} = ExDoc.generate_docs "Elixir", "1", options
    assert source_dir == options[:source_beam]
  end
end  

defmodule ExDocAssetsAndIndexTest do
  use ExUnit.Case
  
  setup do
    :file.set_cwd("test")
    File.mkdir!(output_dir)
    :ok
  end

  teardown do
    System.cmd("rm -rf #{output_dir}")
    :file.set_cwd("..")
    :ok
  end
  
  defp beam_dir do
    Path.expand("../tmp/ebin", __FILE__)
  end
  
  defp output_dir do
    Path.expand("../docs", __FILE__)
  end

  defmodule EmptyRetriever do
    def docs_from_dir(_source, _config), do: []
  end
  
  # Fake data generator
  defmodule FakeRetriever do
    alias ExDoc.ModuleNode
    def docs_from_dir(_source, _config) do
      [ModuleNode[id: :a, module: B, type: nil],
       ModuleNode[id: :c, module: D, type: :behaviour],
       ModuleNode[id: :e, module: Foo, type: :record],
       ModuleNode[id: :g, module: H, type: :exception],
       ModuleNode[id: :i, module: :j, type: :protocol],
       ModuleNode[id: :k, module: L, type: :impl]]
    end
  end
  
  test "run generates .ex_doc_project_info" do
    options = [retriever: FakeRetriever, formatter: ExDocTest.IdentityFormatter,
               source_root: beam_dir, output: output_dir]
    ExDoc.generate_docs "Elixir", "1.2.3.4", options
    assert File.regular?("#{output_dir}/Elixir/.ex_doc_project_info")
    { info, _ } = File.read!("#{output_dir}/Elixir/.ex_doc_project_info")
                  |> Code.eval_string
    exp = [project: "Elixir", version: "1.2.3.4", documented_modules:
            [{B, nil}, {D, :behaviour}, {Foo, :record}, {H, :exception}, {:j, :protocol}]]
    assert info == exp
  end
  
  
  test "run generates assets" do
    options = [retriever: EmptyRetriever,
               source_root: beam_dir, output: output_dir]
    ExDoc.generate_docs "Elixir", "1.2.3.4", options
  
    assert File.regular?("#{output_dir}/_assets/css/style.css")
  end
    
  test "run does not generate assets if create_assets is false" do
    options = [retriever: EmptyRetriever,
               source_root: beam_dir, output: output_dir, create_assets: false]
    ExDoc.generate_docs "Elixir", "1.2.3.4", options
  
    refute File.regular?("#{output_dir}/_assets/css/style.css")
  end
end
