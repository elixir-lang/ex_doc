Code.require_file "../test_helper", __FILE__

defmodule ExDocTest do
  use ExUnit.Case, sync: true

  def setup_all do
    :file.set_cwd("test")
    :file.make_dir(output_dir)
  end

  def teardown_all do
    System.cmd("rm -rf #{output_dir}")
    :file.set_cwd("..")
  end

  defp output_dir do
    File.expand_path("../output", __FILE__)
  end

  test "generate_docs generates the html file with the documentation" do
    ExDoc.generate_docs File.expand_path("tmp")
    assert File.regular?("#{output_dir}/CompiledWithDocs.html")
    assert File.regular?("#{output_dir}/CompiledWithDocs.Nested.html")
  end

  test "generate_docs accepts relative directories" do
    ExDoc.generate_docs "tmp"
    assert File.regular?("#{output_dir}/CompiledWithDocs.html")
    assert File.regular?("#{output_dir}/CompiledWithDocs.Nested.html")
  end

  test "generate_docs generates in specified output directory" do
    ExDoc.generate_docs File.expand_path("../tmp", __FILE__), [output: "#{output_dir}/docs"]
    assert File.regular?("#{output_dir}/docs/CompiledWithDocs.html")
    assert File.regular?("#{output_dir}/docs/index.html")
    assert File.regular?("#{output_dir}/docs/css/style.css")
  end

  test "generate_docs generates all listing files" do
    ExDoc.generate_docs File.expand_path("tmp")

    content = File.read!("#{output_dir}/modules_list.html")
    assert content[%r{<li>.*"CompiledWithDocs\.html".*CompiledWithDocs.*<\/li>}ms]
    assert content[%r{<li>.*"CompiledWithDocs\.html#example\/2".*example\/2.*<\/li>}ms]
    assert content[%r{<li>.*"CompiledWithDocs.Nested\.html".*Nested.*<\/li>}ms]
    assert content[%r{<li>.*"ExDocTest\.Nested\.html".*ExDocTest\.Nested.*<\/li>}ms]
    assert !content[%r{ExDocTest\.Undocumented}ms]

    content = File.read!("#{output_dir}/records_list.html")
    assert content[%r{<li>.*"CompiledRecord\.html".*CompiledRecord.*<\/li>}ms]
    assert content[%r{<li>.*"RandomError\.html".*RandomError.*<\/li>}ms]

    content = File.read!("#{output_dir}/protocols_list.html")
    assert content[%r{<li>.*"CustomProtocol\.html".*CustomProtocol.*<\/li>}ms]
    assert content[%r{<li>.*"CustomProtocol.Number\.html".*Number.*<\/li>}ms]
  end

  test "generate_docs generates the source link with the specified url" do
    ExDoc.generate_docs File.expand_path("../tmp", __FILE__), [project_url: "http://example.com/bar"]
    content = File.read!("#{output_dir}/CompiledWithDocs.html")
    assert content[%r{http:\/\/example.com\/bar}m]
  end
end
