defmodule ExDoc.HTMLFormatterTest do
  use ExUnit.Case

  alias ExDoc.HTMLFormatter

  setup_all do
    :file.set_cwd("test")
    :file.make_dir(output_dir)
    :ok
  end

  teardown_all do
    System.cmd("rm -rf #{output_dir}")
    :file.set_cwd("..")
    :ok
  end
  
  defp output_dir do
    Path.expand("../docs", __DIR__)
  end

  defp proj_output_dir do
    Path.expand("../../docs/Elixir", __FILE__)
  end

  defp beam_dir do
    Path.expand("../tmp/ebin", __DIR__)
  end

  defp doc_config do
    ExDoc.Config[project: "Elixir", version: "1.0.1", source_root: beam_dir,
                 output: output_dir]
  end

  defp get_modules(config // doc_config) do
    ExDoc.Retriever.docs_from_dir(beam_dir, config)
  end

  test "run generates the html file with the documentation" do
    HTMLFormatter.run(get_modules, doc_config)

    assert File.regular?("#{proj_output_dir}/CompiledWithDocs.html")
    assert File.regular?("#{proj_output_dir}/CompiledWithDocs.Nested.html")
  end

  test "run generates in specified output directory" do
    File.mkdir_p!("#{output_dir}/foo/bar")
    config = ExDoc.Config[project: "bar", output: "#{output_dir}/foo"]
    HTMLFormatter.run(get_modules(config), config)

    assert File.regular?("#{output_dir}/foo/bar/CompiledWithDocs.html")
    assert File.regular?("#{output_dir}/foo/bar/index.html")
  end

  test "run generates all listing files" do
    HTMLFormatter.run(get_modules, doc_config)

    content = File.read!("#{proj_output_dir}/modules_list.html")
    assert content =~ %r{<li>.*"CompiledWithDocs\.html".*CompiledWithDocs.*<\/li>}ms
    assert content =~ %r{<li>.*"CompiledWithDocs\.html#example\/2".*example\/2.*<\/li>}ms
    assert content =~ %r{<li>.*"CompiledWithDocs.Nested\.html".*Nested.*<\/li>}ms
    assert content =~ %r{<li>.*"UndefParent\.Nested\.html".*UndefParent\.Nested.*<\/li>}ms
    assert content =~ %r{<li>.*"CustomBehaviour.html".*CustomBehaviour.*<\/li>}ms
    refute content =~ %r{UndefParent\.Undocumented}ms

    content = File.read!("#{proj_output_dir}/records_list.html")
    assert content =~ %r{<li>.*"CompiledRecord\.html".*CompiledRecord.*<\/li>}ms
    assert content =~ %r{<li>.*"RandomError\.html".*RandomError.*<\/li>}ms

    content = File.read!("#{proj_output_dir}/protocols_list.html")
    assert content =~ %r{<li>.*"CustomProtocol\.html".*CustomProtocol.*<\/li>}ms
  end

  test "run generates the overview file" do
    File.mkdir!(proj_output_dir)
    HTMLFormatter.run(get_modules, doc_config)

    assert File.regular?("#{proj_output_dir}/overview.html")
    content = File.read!("#{proj_output_dir}/overview.html")
    assert content =~ %r{<a href="CompiledWithDocs.html">CompiledWithDocs</a>}
    assert content =~ %r{<p>moduledoc</p>}
    assert content =~ %r{<a href="CompiledWithDocs.Nested.html">CompiledWithDocs.Nested</a>}
  end
end
