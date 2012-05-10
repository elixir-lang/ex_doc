Code.require_file "../../test_helper", __FILE__

defmodule ExDoc.HTMLFormatterTest do
  use ExUnit.Case

  defp input_path do
    File.expand_path("test/tmp/__MAIN__")
  end

  defp source_root_url do
    "https://github.com/elixir-lang/elixir/blob/master"
  end

  defp get_content(kind, names) do
    files  = Enum.map names, fn(n) -> "#{input_path}/#{n}.beam" end
    [node] = Keyword.get ExDoc.Retriever.get_docs(files, input_path, "#{source_root_url}/%{path}#L%{line}"), kind
    ExDoc.HTMLFormatter.module_page(node)
  end

  ## MODULES

  test "module_page generate only the module name when there's no more info" do
    node = ExDoc.ModuleNode.new module: XPTOModule, moduledoc: nil, id: "XPTOModule"
    content = ExDoc.HTMLFormatter.module_page(node)

    assert content[%r/<title>XPTOModule<\/title>/]
    assert content[%r/<h1>\s*XPTOModule\s*<\/h1>/]
  end

  test "module_page outputs the docstrings" do
    content = get_content(:modules, ["CompiledWithDocs"])

    assert content[%r/<title>CompiledWithDocs<\/title>/]
    assert content[%r/<h1>\s*CompiledWithDocs\s*<\/h1>/]
    assert content[%r/moduledoc.*Example.*CompiledWithDocs\.example.*/m]
    assert content[%r/example\/0.*Some example/m]
    assert content[%r/example_without_docs\/0.*<div class="docstring">.*<\/div>/m]
    assert content[%r/example_1\/0.*Another example/m]
    assert content[%r{<p class="signature" id="example_1/0">}]
    assert content[%r{<a href="#{source_root_url}/test/fixtures/compiled_with_docs.ex#L10"[^>]*>Source<\/a>}m]
  end

  test "module_page outputs summaries" do
    content = get_content(:modules, ["CompiledWithDocs"])
    assert content[%r{<span class="summary_signature">\s*<a href="#example_1/0">}]
  end

  ## RECORDS

  test "module_page outputs the record type" do
    content = get_content(:records, ["CompiledRecord"])
    assert content[%r{<h1>\s*CompiledRecord\s*<small>record</small>\s*<\/h1>}m]
  end

  test "module_page outputs record fields" do
    content = get_content(:records, ["CompiledRecord"])
    assert content[%r{<strong>foo:</strong> nil}m]
    assert content[%r{<strong>bar:</strong> "sample"}m]
  end

  test "module_page outputs exceptions fields" do
    content = get_content(:records, ["RandomError"])
    assert !content[%r{<strong>__exception__:</strong>}m]
    assert content[%r{<strong>message:</strong> "this is random!"}m]
  end

  ## PROTOCOLS

  test "module_page outputs the protocol type" do
    content = get_content(:protocols, ["CustomProtocol"])
    assert content[%r{<h1>\s*CustomProtocol\s*<small>protocol</small>\s*<\/h1>}m]
  end

  test "module_page outputs protocol implementations" do
    content = get_content(:protocols, ["CustomProtocol", "CustomProtocol/Number"])
    assert content[%r{<a href="CustomProtocol.Number.html">Number</a>}m]
  end

  ## LISTING

  test "current listing page is marked as selected" do
    content = ExDoc.HTMLFormatter.list_page(:modules, [])
    assert content[%r{<span class="selected"><a target="_self" href="modules_list.html">}]
    assert content[%r{<span class=""><a target="_self" href="records_list.html">}]
  end

  test "list_page outputs listing for the given nodes" do
    files   = [
      "#{input_path}/CompiledWithDocs.beam",
      "#{input_path}/CompiledWithDocs/Nested.beam"
    ]
    nodes   = Keyword.get ExDoc.Retriever.get_docs(files, input_path, source_root_url), :modules
    content = ExDoc.HTMLFormatter.list_page(:modules, nodes)

    assert content[%r{<li>.*"CompiledWithDocs\.html".*CompiledWithDocs.*<\/li>}m]
    assert content[%r{<li>.*"CompiledWithDocs\.html#example\/0".*example\/0.*<\/li>}m]
    assert content[%r{<li>.*"CompiledWithDocs\.html#example_1\/0".*example_1\/0.*<\/li>}m]
    assert content[%r{<li>.*"CompiledWithDocs\.html#example_without_docs\/0".*example_without_docs\/0.*<\/li>}m]
    assert content[%r{<li>.*"CompiledWithDocs.Nested\.html".*Nested.*<\/li>}m]
  end
end
