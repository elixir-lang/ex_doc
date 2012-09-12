Code.require_file "../../test_helper.exs", __FILE__

defmodule ExDoc.HTMLFormatterTest do
  use ExUnit.Case

  defp input_path do
    File.expand_path("test/tmp/Elixir")
  end

  defp source_root_url do
    "https://github.com/elixir-lang/elixir/blob/master"
  end

  defp get_content(kind, names) do
    files  = Enum.map names, fn(n) -> "#{input_path}-#{n}.beam" end
    [node] = Keyword.get ExDoc.Retriever.get_docs(files, "#{source_root_url}/%{path}#L%{line}"), kind
    ExDoc.HTMLFormatter.module_page(node)
  end

  ## MODULES

  test "module_page generates only the module name when there's no more info" do
    node = ExDoc.ModuleNode.new module: XPTOModule, moduledoc: nil, id: "XPTOModule"
    content = ExDoc.HTMLFormatter.module_page(node)

    assert content =~ %r/<title>XPTOModule<\/title>/
    assert content =~ %r/<h1>\s*XPTOModule\s*<\/h1>/
  end

  test "module_page outputs the functions and docstrings" do
    content = get_content(:modules, ["CompiledWithDocs"])

    assert content =~ %r/<title>CompiledWithDocs<\/title>/
    assert content =~ %r/<h1>\s*CompiledWithDocs\s*<\/h1>/
    assert content =~ %r/moduledoc.*Example.*CompiledWithDocs\.example.*/ms
    assert content =~ %r/example\/2.*Some example/ms
    assert content =~ %r/example_without_docs\/0.*<div class="docstring">.*<\/div>/ms
    assert content =~ %r/example_1\/0.*Another example/ms
    assert content =~ %r{<p class="signature" id="example_1/0">}
    assert content =~ %r{<strong>example\(foo, bar // Baz\)</strong>}
    assert content =~ %r{<a href="#{source_root_url}/test/fixtures/compiled_with_docs.ex#L10"[^>]*>Source<\/a>}ms
  end

  test "module_page outputs summaries" do
    content = get_content(:modules, ["CompiledWithDocs"])
    assert content =~ %r{<span class="summary_signature">\s*<a href="#example_1/0">}
  end

  ## BEHAVIOURS

  test "module_page outputs behavior and callbacks" do
    content = get_content(:modules, ["CustomBehaviour"])

    assert content =~ %r{<h1>\s*CustomBehaviour\s*<small>behaviour</small>\s*<\/h1>}m
    assert content =~ %r{Callbacks}
    assert content =~ %r{<p class="signature" id="hello/1">}
  end

  ## RECORDS

  test "module_page outputs the record type" do
    content = get_content(:records, ["CompiledRecord"])
    assert content =~ %r{<h1>\s*CompiledRecord\s*<small>record</small>\s*<\/h1>}m
  end

  test "module_page outputs record fields" do
    content = get_content(:records, ["CompiledRecord"])
    assert content =~ %r{<strong>foo:</strong> nil}m
    assert content =~ %r{<strong>bar:</strong> "sample"}m
  end

  test "module_page outputs exceptions fields" do
    content = get_content(:records, ["RandomError"])
    refute content =~ %r{<strong>__exception__:</strong>}m
    assert content =~ %r{<strong>message:</strong> "this is random!"}m
  end

  ## PROTOCOLS

  test "module_page outputs the protocol type" do
    content = get_content(:protocols, ["CustomProtocol"])
    assert content =~ %r{<h1>\s*CustomProtocol\s*<small>protocol</small>\s*<\/h1>}m
  end

  test "module_page outputs protocol implementations" do
    content = get_content(:protocols, ["CustomProtocol", "CustomProtocol-Number"])
    assert content =~ %r{<a href="CustomProtocol.Number.html">Number</a>}m
  end

  ## LISTING

  test "current listing page is marked as selected" do
    content = ExDoc.HTMLFormatter.list_page(:modules, [])
    assert content =~ %r{<span class="selected"><a target="_self" href="modules_list.html">}
    assert content =~ %r{<span class=""><a target="_self" href="records_list.html">}
  end

  test "list_page outputs listing for the given nodes" do
    files   = [
      "#{input_path}-CompiledWithDocs.beam",
      "#{input_path}-CompiledWithDocs-Nested.beam"
    ]
    nodes   = Keyword.get ExDoc.Retriever.get_docs(files, source_root_url), :modules
    content = ExDoc.HTMLFormatter.list_page(:modules, nodes)

    assert content =~ %r{<li>.*"CompiledWithDocs\.html".*CompiledWithDocs.*<\/li>}ms
    assert content =~ %r{<li>.*"CompiledWithDocs\.html#example\/2".*example\/2.*<\/li>}ms
    assert content =~ %r{<li>.*"CompiledWithDocs\.html#example_1\/0".*example_1\/0.*<\/li>}ms
    assert content =~ %r{<li>.*"CompiledWithDocs\.html#example_without_docs\/0".*example_without_docs\/0.*<\/li>}ms
    assert content =~ %r{<li>.*"CompiledWithDocs.Nested\.html".*Nested.*<\/li>}ms
  end
end
