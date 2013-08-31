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
    Path.expand("../../docs", __FILE__)
  end

  defp beam_dir do
    Path.expand("../../tmp/ebin", __FILE__)
  end
  
  defp fixture_dir do
    Path.expand("../../fixtures", __FILE__)
  end

  defp doc_config do
    ExDoc.Config[project: "Elixir", version: "1.0.1", source_root: beam_dir,
      guide_files: [ "#{fixture_dir}/doc.md", "#{fixture_dir}/docdir" ] ]
  end

  defp get_modules(config // doc_config) do
    ExDoc.Retriever.docs_from_dir(beam_dir, config)
  end

  test "run generates the html file with the documentation" do
    HTMLFormatter.run(get_modules, doc_config)

    assert File.regular?("#{output_dir}/CompiledWithDocs.html")
    assert File.regular?("#{output_dir}/CompiledWithDocs.Nested.html")
  end

  test "run generates in specified output directory" do
    config = ExDoc.Config[output: "#{output_dir}/docs"]
    HTMLFormatter.run(get_modules(config), config)

    assert File.regular?("#{output_dir}/docs/CompiledWithDocs.html")
    assert File.regular?("#{output_dir}/docs/index.html")
    assert File.regular?("#{output_dir}/docs/css/style.css")
  end

  test "run generates all listing files" do
    HTMLFormatter.run(get_modules, doc_config)

    content = File.read!("#{output_dir}/modules_list.html")
    assert content =~ %r{<li>.*"CompiledWithDocs\.html".*CompiledWithDocs.*<\/li>}ms
    assert content =~ %r{<li>.*"CompiledWithDocs\.html#example\/2".*example\/2.*<\/li>}ms
    assert content =~ %r{<li>.*"CompiledWithDocs.Nested\.html".*Nested.*<\/li>}ms
    assert content =~ %r{<li>.*"UndefParent\.Nested\.html".*UndefParent\.Nested.*<\/li>}ms
    assert content =~ %r{<li>.*"CustomBehaviour.html".*CustomBehaviour.*<\/li>}ms
    refute content =~ %r{UndefParent\.Undocumented}ms

    content = File.read!("#{output_dir}/records_list.html")
    assert content =~ %r{<li>.*"CompiledRecord\.html".*CompiledRecord.*<\/li>}ms
    assert content =~ %r{<li>.*"RandomError\.html".*RandomError.*<\/li>}ms

    content = File.read!("#{output_dir}/protocols_list.html")
    assert content =~ %r{<li>.*"CustomProtocol\.html".*CustomProtocol.*<\/li>}ms
    
    content = File.read!("#{output_dir}/guide_list.html")
    assert content =~ %r{<li>.*/guide/doc\.html".*For the guide.*<\/li>}ms
    assert content =~ %r{<li>.*/guide/a\.html".*This is file A.*<\/li>}ms
    assert content =~ %r{<li>.*/guide/b.html".*This is file B.*<\/li>}ms
  end
end
