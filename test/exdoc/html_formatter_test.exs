Code.require_file "../../test_helper", __FILE__

defmodule ExDoc.HTMLFormatterTest do
  use ExUnit.Case

  defp input_path do
    File.expand_path("test/tmp")
  end

  defp source_root_url do
    "https://github.com/elixir-lang/elixir/blob/master/"
  end

  test "module_page generate only the module name when there's no more info" do
    node = ExDoc.Node.new module: XPTOModule, moduledoc: {1, nil}
    content = ExDoc.HTMLFormatter.module_page(node)

    assert content[%r/<title>XPTOModule<\/title>/]
    assert content[%r/<h1>XPTOModule<\/h1>/]
  end

  test "module_page outputs the docstrings" do
    file = "#{input_path}/CompiledWithDocs.beam"
    [node] = ExDoc.Retriever.get_docs [file], input_path
    content = ExDoc.HTMLFormatter.module_page(node)

    assert content[%r/<title>CompiledWithDocs<\/title>/]
    assert content[%r/<h1>CompiledWithDocs<\/h1>/]
    assert content[%r/moduledoc.*Example.*CompiledWithDocs\.example.*/m]
    assert content[%r/example\/0.*Some example/m]
    assert content[%r/example_without_docs\/0.*<div class="docstring">.*<\/div>/m]
    assert content[%r/example_1\/0.*Another example/m]
    assert content[%r{<p class="signature" id="example_1/0">}]
    assert content[%r{<a href="#{source_root_url}test/fixtures/compiled_with_docs.ex#L10"[^>]*>Source<\/a>}m]
  end

  test "module_page outputs summaries" do
    file = "#{input_path}/CompiledWithDocs.beam"
    [node] = ExDoc.Retriever.get_docs [file], input_path
    content = ExDoc.HTMLFormatter.module_page(node)
    assert content[%r{<span class="summary_signature">\s*<a href="#example_1/0">}]
  end
end
