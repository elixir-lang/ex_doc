defmodule ExDoc.Formatter.HTML.TemplatesTest do
  use ExUnit.Case, async: true

  alias ExDoc.Formatter.HTML.Templates

  defp source_url do
    "https://github.com/elixir-lang/elixir"
  end

  defp homepage_url do
    "http://elixir-lang.org"
  end

  defp doc_config do
    %ExDoc.Config{project: "Elixir", version: "1.0.1", source_root: File.cwd!,
                  source_url_pattern: "#{source_url}/blob/master/%{path}#L%{line}",
                  homepage_url: homepage_url, source_url: source_url}
  end

  defp get_module_page(names) do
    mods = names
           |> ExDoc.Retriever.docs_from_modules(doc_config)
           |> ExDoc.Formatter.HTML.Autolink.all()

    Templates.module_page(hd(mods), doc_config, mods)
  end

  ## LISTING

  test "current listing page is marked as selected" do
    content = Templates.list_template(:modules, [], doc_config, false)
    assert content =~ ~r{<span class="selected"><a target="_self" href="modules_list.html">}
    assert content =~ ~r{<span class=""><a target="_self" href="exceptions_list.html">}
  end

  test "site title text links to homepage_url when set" do
    content = Templates.list_template(:modules, [], doc_config, false)
    assert content =~ ~r{<a href="#{homepage_url}" target="_top">Elixir v1.0.1</a>}
  end

  test "site title text links to source_url when there is no homepage_url" do
    doc_config_without_source_url = %ExDoc.Config{project: "Elixir", version: "1.0.1", source_root: File.cwd!,
                                                  source_url: source_url,
                                                  source_url_pattern: "#{source_url}/blob/master/%{path}#L%{line}"}
    content = Templates.list_template(:modules, [], doc_config_without_source_url, false)
    assert content =~ ~r{<a href="#{source_url}" target="_top">Elixir v1.0.1</a>}
  end

  test "site title text creates no link when there is no homepage_url or source_url" do
    doc_config_without_source_url = %ExDoc.Config{project: "Elixir", version: "1.0.1", source_root: File.cwd!,
                                                  source_url_pattern: "#{source_url}/blob/master/%{path}#L%{line}"}
    content = Templates.list_template(:modules, [], doc_config_without_source_url, false)
    assert content =~ ~r{Elixir v1.0.1}
  end

  test "list_page outputs listing for the given nodes" do
    names = [CompiledWithDocs, CompiledWithDocs.Nested]
    nodes = ExDoc.Retriever.docs_from_modules(names, doc_config)
    content = Templates.list_template(:modules, nodes, doc_config, false)

    assert content =~ ~r{<li>.*"CompiledWithDocs\.html".*CompiledWithDocs.*<\/li>}ms
    assert content =~ ~r{<li>.*"CompiledWithDocs\.html#example\/2".*example\/2.*<\/li>}ms
    assert content =~ ~r{<li>.*"CompiledWithDocs\.html#example_1\/0".*example_1\/0.*<\/li>}ms
    assert content =~ ~r{<li>.*"CompiledWithDocs\.html#example_without_docs\/0".*example_without_docs\/0.*<\/li>}ms
    assert content =~ ~r{<li>.*"CompiledWithDocs.Nested\.html".*Nested.*<\/li>}ms
  end

  test "listing page has README link if present" do
    content = Templates.list_template(:modules, [], doc_config, true)
    assert content =~ ~r{<a href="README.html">README</a>}
  end

  test "listing page doesn't have README link if not present" do
    content = Templates.list_template(:modules, [], doc_config, false)
    refute content =~ ~r{<a href="README.html">README</a>}
  end

  ## LIST ITEMS

  test "arrows for modules with functions" do
    [node] = ExDoc.Retriever.docs_from_modules([FunctionModule], doc_config)
    content = Templates.list_item_template(node)
    assert content =~ ~r/<a class="toggle">/
  end

  test "no arrows for modules without functions" do
    [node] = ExDoc.Retriever.docs_from_modules([NoFunctionsModule], doc_config)
    content = Templates.list_item_template(node)
    refute content =~ ~r/<a class="toggle">/
  end

  ## MODULES

  test "module_page generates only the module name when there's no more info" do
    node = %ExDoc.ModuleNode{module: XPTOModule, moduledoc: nil, id: "XPTOModule"}
    content = Templates.module_page(node, doc_config, [node])

    assert content =~ ~r/<title>XPTOModule<\/title>/
    assert content =~ ~r/<h1>\s*XPTOModule\s*<\/h1>/
  end

  test "module_page outputs the functions and docstrings" do
    content = get_module_page([CompiledWithDocs])

    assert content =~ ~r/<title>CompiledWithDocs<\/title>/
    assert content =~ ~r/<h1>\s*CompiledWithDocs\s*<\/h1>/
    assert content =~ ~r/moduledoc.*Example.*CompiledWithDocs\.example.*/ms
    assert content =~ ~r/example\/2.*Some example/ms
    assert content =~ ~r/example_without_docs\/0.*<div class="docstring">.*<\/div>/ms
    assert content =~ ~r/example_1\/0.*Another example/ms
    assert content =~ ~r{<a href="#{source_url}/blob/master/test/fixtures/compiled_with_docs.ex#L10"[^>]*>Source<\/a>}ms

    assert content =~ ~s{<div class="detail_header" id="example_1/0">}
    assert content =~ ~s{<strong>example(foo, bar \\\\ Baz)</strong>}
    assert content =~ ~s{<span class="detail_type">\(function\)</span>}
    assert content =~ ~s{<a href="#example/2" class="detail_link" title="Link to this function">#</a>}
    assert content =~ ~s{<a class="to_top_link" href="#content" title="To the top of the page">&uarr;</a>}
  end

  test "module_page outputs the types and function specs" do
    content = get_module_page([TypesAndSpecs, TypesAndSpecs.Sub])

    mb = "http://elixir-lang.org/docs/stable"

    public_html =
      "<a href=\"#t:public/1\">public(t)</a> :: {t, " <>
      "<a href=\"#{mb}/elixir/String.html#t:t/0\">String.t</a>, " <>
      "<a href=\"TypesAndSpecs.Sub.html#t:t/0\">TypesAndSpecs.Sub.t</a>, " <>
      "<a href=\"#t:opaque/0\">opaque</a>, :ok | :error}"

    ref_html = "<a href=\"#t:ref/0\">ref</a> :: " <>
               "{:binary.part, <a href=\"#t:public/1\">public(any)</a>}"

    assert content =~ ~s[<a href="#t:public/1">public(t)</a>]
    refute content =~ ~s[<a href="#t:private/0">private</a>]
    assert content =~ public_html
    assert content =~ ref_html
    refute content =~ ~s[<strong>private\(t\)]
    assert content =~ ~s[A public type]
    assert content =~ ~s[add(integer, <a href="#t:opaque/0">opaque</a>) :: integer]
    refute content =~ ~s[minus(integer, integer) :: integer]
  end

  test "module_page outputs summaries" do
    content = get_module_page([CompiledWithDocs])
    assert content =~ ~r{<td class="summary_signature">\s*<a href="#example_1/0">}
  end

  test "module_page contains breadcrumbs" do
    content = get_module_page([CompiledWithDocs])
    assert content =~ ~s{<div class="breadcrumbs">}
    assert content =~ ~s{Elixir v1.0.1 &rarr; <a href="overview.html">Overview</a> } <>
                      ~s{&rarr; <a href="CompiledWithDocs.html">CompiledWithDocs</a>}
  end

  test "module_page breadcrumbs do not link to non-existent pages" do
    content = get_module_page([UndefParent.Nested])
    assert content =~ ~r{&rarr; UndefParent &rarr; <a href="UndefParent.Nested.html">Nested</a>}
  end

  test "module_page contains links to summary sections when those exist" do
    content = get_module_page([CompiledWithDocs, CompiledWithDocs.Nested])
    assert content =~ ~r{<a href="#summary">Summary</a>}
    refute content =~ ~r{types_details}
  end

  ## BEHAVIOURS

  test "module_page outputs behavior and callbacks" do
    content = get_module_page([CustomBehaviour])

    assert content =~ ~r{<h1>\s*CustomBehaviour\s*<small>behaviour</small>\s*</h1>}m
    assert content =~ ~r{Callbacks}
    assert content =~ ~r{<div class="detail_header" id="c:hello/1">}
  end

  ## PROTOCOLS

  test "module_page outputs the protocol type" do
    content = get_module_page([CustomProtocol])
    assert content =~ ~r{<h1>\s*CustomProtocol\s*<small>protocol</small>\s*</h1>}m
  end
end
