Code.require_file "../../test_helper", __FILE__

defmodule ExDoc.HTMLFormatterTest do
  use ExUnit.Case, sync: true

  def setup_all do
    :file.make_dir(output_dir)
  end

  def teardown_all do
    :os.cmd('rm -rf #{output_dir}')
  end

  defp output_dir do
    File.expand_path "test/tmp/output"
  end

  defp source_root_url do
    "https://github.com/elixir-lang/elixir/blob/master/"
  end

  test "format_docs generate only the module name when there's no more info" do
    node = ExDoc.Node.new name: "XPTOModule", moduledoc: {1, nil}
    ExDoc.HTMLFormatter.format_docs(node, output_dir)

    content = File.read!("#{output_dir}/XPTOModule.html")
    assert content[%r/<title>XPTOModule<\/title>/]
    assert content[%r/<h1>XPTOModule<\/h1>/]
  end

  test "generate_docs outputs the correct content" do
    input_path = File.expand_path "test/tmp"
    file = "#{input_path}/CompiledWithDocs.beam"

    [docs] = ExDoc.Retriever.get_docs [file], input_path
    ExDoc.HTMLFormatter.format_docs(docs, output_dir)

    content = File.read!("#{output_dir}/CompiledWithDocs.html")
    assert content[%r/<title>CompiledWithDocs<\/title>/]
    assert content[%r/<h1>CompiledWithDocs<\/h1>/]
    assert content[%r/moduledoc.*Example.*CompiledWithDocs\.example.*/m]
    assert content[%r/example\/0.*Some example/m]
    assert content[%r/example_without_docs\/0.*<div class="description">.*<\/div>/m]
    assert content[%r/example_1\/0.*Another example/m]
    assert content[%r{<a href="#{source_root_url}test/fixtures/compiled_with_docs.ex#L10"[^>]*>Source<\/a>}m]
  end
end
