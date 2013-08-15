Code.require_file "../../test_helper.exs", __FILE__

defmodule ExDoc.HTMLFormatterTest do
  use ExUnit.Case

  defp input_path do
    Path.expand("test/tmp/Elixir")
  end

  defp source_url do
    "https://github.com/elixir-lang/elixir"
  end

  defp homepage_url do
    "http://elixir-lang.org"
  end

  defp doc_config do
    ExDoc.Config[project: "Elixir", version: "1.0.1", source_root: File.cwd!,
                 source_url_pattern: "#{source_url}/blob/master/%{path}#L%{line}",
                 homepage_url: homepage_url, source_url: source_url]
  end

  defp get_content(names) do
    files  = Enum.map names, fn(n) -> "#{input_path}.#{n}.beam" end
    [node] = ExDoc.Retriever.docs_from_files(files, doc_config)
    ExDoc.HTMLFormatter.module_page(node, doc_config)
  end

  ## MODULES

  test "module_page generates only the module name when there's no more info" do
    node = ExDoc.ModuleNode.new module: XPTOModule, moduledoc: nil, id: "XPTOModule"
    content = ExDoc.HTMLFormatter.module_page(node, doc_config)

    assert content =~ %r/<title>XPTOModule<\/title>/
    assert content =~ %r/<h1>\s*XPTOModule\s*<\/h1>/
  end

  test "module_page outputs the functions and docstrings" do
    content = get_content(["CompiledWithDocs"])

    assert content =~ %r/<title>CompiledWithDocs<\/title>/
    assert content =~ %r/<h1>\s*CompiledWithDocs\s*<\/h1>/
    assert content =~ %r/moduledoc.*Example.*CompiledWithDocs\.example.*/ms
    assert content =~ %r/example\/2.*Some example/ms
    assert content =~ %r/example_without_docs\/0.*<div class="docstring">.*<\/div>/ms
    assert content =~ %r/example_1\/0.*Another example/ms
    assert content =~ %r{<p class="signature" id="example_1/0">}
    assert content =~ %r{<strong>example\(foo, bar // Baz\)</strong>}
    assert content =~ %r{<a href="#{source_url}/blob/master/test/fixtures/compiled_with_docs.ex#L10"[^>]*>Source<\/a>}ms
  end

  test "module_page outputs summaries" do
    content = get_content(["CompiledWithDocs"])
    assert content =~ %r{<span class="summary_signature">\s*<a href="#example_1/0">}
  end

  ## BEHAVIOURS

  test "module_page outputs behavior and callbacks" do
    content = get_content(["CustomBehaviour"])

    assert content =~ %r{<h1>\s*CustomBehaviour\s*<small>behaviour</small>\s*<\/h1>}m
    assert content =~ %r{Callbacks}
    assert content =~ %r{<p class="signature" id="hello/1">}
  end

  ## RECORDS

  test "module_page outputs the record type" do
    content = get_content(["CompiledRecord"])
    assert content =~ %r{<h1>\s*CompiledRecord\s*<small>record</small>\s*<\/h1>}m
  end

  test "module_page outputs record fields" do
    content = get_content(["CompiledRecord"])
    assert content =~ %r{<strong>foo:</strong> nil}m
    assert content =~ %r{<strong>bar:</strong> "sample"}m
  end

  test "module_page outputs exceptions fields" do
    content = get_content(["RandomError"])
    refute content =~ %r{<strong>__exception__:</strong>}m
    assert content =~ %r{<strong>message:</strong> "this is random!"}m
  end

  ## PROTOCOLS

  test "module_page outputs the protocol type" do
    content = get_content(["CustomProtocol"])
    assert content =~ %r{<h1>\s*CustomProtocol\s*<small>protocol</small>\s*<\/h1>}m
  end

  test "module_page outputs protocol implementations" do
    names = ["CustomProtocol", "CustomProtocol.Number"]
    files  = Enum.map names, fn(n) -> "#{input_path}.#{n}.beam" end
    nodes = ExDoc.Retriever.docs_from_files(files, doc_config)
    [node] = ExDoc.Retriever.nest_modules(nodes, doc_config)
    content = ExDoc.HTMLFormatter.module_page(node, doc_config)
    assert content =~ %r{<a href="CustomProtocol.Number.html">Number</a>}m
  end

end
