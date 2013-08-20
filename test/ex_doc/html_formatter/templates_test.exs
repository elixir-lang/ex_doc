defmodule ExDoc.HTMLFormatter.TemplatesTest do
  use ExUnit.Case, async: true

  alias ExDoc.HTMLFormatter.Templates

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

  defp get_module_page(names) do
    names
    |> ExDoc.Retriever.docs_from_modules(doc_config)
    |> ExDoc.HTMLFormatter.Autolink.all()
    |> hd()
    |> Templates.module_page
  end

  ## LISTING

  test "current listing page is marked as selected" do
    content = Templates.list_template(:modules, [], doc_config, false)
    assert content =~ %r{<span class="selected"><a target="_self" href="modules_list.html">}
    assert content =~ %r{<span class=""><a target="_self" href="records_list.html">}
  end

  test "site title text links to homepage_url when set" do
    content = Templates.list_template(:modules, [], doc_config, false)
    assert content =~ %r{<a href="#{homepage_url}" target="_blank">Elixir v1.0.1</a>}
  end

  test "site title text links to source_url when there is no homepage_url" do
    doc_config_without_source_url = ExDoc.Config[project: "Elixir", version: "1.0.1", source_root: File.cwd!,
                                                 source_url: source_url,
                                                 source_url_pattern: "#{source_url}/blob/master/%{path}#L%{line}"]
    content = Templates.list_template(:modules, [], doc_config_without_source_url, false)
    assert content =~ %r{<a href="#{source_url}" target="_blank">Elixir v1.0.1</a>}
  end

  test "site title text links to / when there is no homepage_url or source_url" do
    doc_config_without_source_url = ExDoc.Config[project: "Elixir", version: "1.0.1", source_root: File.cwd!,
                                                 source_url_pattern: "#{source_url}/blob/master/%{path}#L%{line}"]
    content = Templates.list_template(:modules, [], doc_config_without_source_url, false)
    assert content =~ %r{<a href="/" target="_blank">Elixir v1.0.1</a>}
  end

  test "list_page outputs listing for the given nodes" do
    names = [CompiledWithDocs, CompiledWithDocs.Nested]
    nodes   = ExDoc.Retriever.docs_from_modules(names, doc_config)
    content = Templates.list_template(:modules, nodes, doc_config, false)

    assert content =~ %r{<li>.*"CompiledWithDocs\.html".*CompiledWithDocs.*<\/li>}ms
    assert content =~ %r{<li>.*"CompiledWithDocs\.html#example\/2".*example\/2.*<\/li>}ms
    assert content =~ %r{<li>.*"CompiledWithDocs\.html#example_1\/0".*example_1\/0.*<\/li>}ms
    assert content =~ %r{<li>.*"CompiledWithDocs\.html#example_without_docs\/0".*example_without_docs\/0.*<\/li>}ms
    assert content =~ %r{<li>.*"CompiledWithDocs.Nested\.html".*Nested.*<\/li>}ms
  end

  test "listing page has README link if present" do
    content = Templates.list_template(:modules, [], doc_config, true)
    assert content =~ %r{<a href="README.html">README</a>}
  end

  test "listing page doesn't have README link if not present" do
    content = Templates.list_template(:modules, [], doc_config, false)
    refute content =~ %r{<a href="README.html">README</a>}
  end

  ## LIST ITEMS

  test "arrows for modules with functions" do
    defmodule FunctionModule do
      def func, do: true
    end

    [node] = ExDoc.Retriever.docs_from_modules([FunctionModule], doc_config)
    content = Templates.list_item_template(node)
    assert content =~ %r/<a class="toggle">/
  end

  test "no arrows for modules without functions" do
    defmodule NoFunctionsModule do
    end

    [node] = ExDoc.Retriever.docs_from_modules([NoFunctionsModule], doc_config)
    content = Templates.list_item_template(node)
    refute content =~ %r/<a class="toggle">/
  end

  test "no arrows for records without functions" do
    defrecord NoFunRecord, value: true

    [node]  = ExDoc.Retriever.docs_from_modules([NoFunRecord], doc_config)
    content = Templates.list_item_template(node)
    refute content =~ %r/<a class="toggle">/
  end

  test "arrows for records with functions" do
    defrecord FunRecord, value: true do
      def record_fun, do: true
    end

    [node] = ExDoc.Retriever.docs_from_modules([FunRecord], doc_config)
    content = Templates.list_item_template(node)
    assert content =~ %r/<a class="toggle">/
  end

  test "no arrows for exceptions without functions" do
    defexception NoFunException, message: "No functions"

    [node] = ExDoc.Retriever.docs_from_modules([NoFunException], doc_config)
    content = Templates.list_item_template(node)
    refute content =~ %r/<a class="toggle">/
  end

  test "arrows for exceptions with functions" do
    defexception FunException, message: "No functions" do
      def exception_fun, do: true
    end

    [node] = ExDoc.Retriever.docs_from_modules([FunException], doc_config)
    content = Templates.list_item_template(node)
    assert content =~ %r/<a class="toggle">/
  end

  ## MODULES

  test "module_page generates only the module name when there's no more info" do
    node = ExDoc.ModuleNode.new module: XPTOModule, moduledoc: nil, id: "XPTOModule"
    content = Templates.module_page(node)

    assert content =~ %r/<title>XPTOModule<\/title>/
    assert content =~ %r/<h1>\s*XPTOModule\s*<\/h1>/
  end

  test "module_page outputs the functions and docstrings" do
    content = get_module_page([CompiledWithDocs])

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
  
  test "module_page outputs the types and function specs" do
    content = get_module_page([TypesAndSpecs, TypesAndSpecs.Sub])

    mb = "http://elixir-lang.org/docs/master"

    public_html = 
      "<a href=\"#t:public/1\">public(t)</a> :: {t, " <>
      "<a href=\"#{mb}/String.html#t:t/0\">String.t()</a>, " <>
      "<a href=\"TypesAndSpecs.Sub.html#t:t/0\">TypesAndSpecs.Sub.t()</a>, " <>
      "<a href=\"#t:opaque/0\">opaque()</a>, :ok | :error}"

    ref_html = "<a href=\"#t:ref/0\">ref()</a> :: " <>
               "{:binary.part(), <a href=\"#t:public/1\">public(any())</a>}"

    assert content =~ %b[<a href="#t:public/1">public(t)</a>]
    refute content =~ %b[<a href="#t:private/0">private</a>]
    assert content =~ public_html
    assert content =~ ref_html
    refute content =~ %b[<strong>private\(t\)]
    assert content =~ %b[add(integer(), <a href="#t:opaque/0">opaque()</a>) :: integer()]
    refute content =~ %b[minus(integer(), integer()) :: integer()]
  end

  test "module_page outputs summaries" do
    content = get_module_page([CompiledWithDocs])
    assert content =~ %r{<span class="summary_signature">\s*<a href="#example_1/0">}
  end

  ## BEHAVIOURS

  test "module_page outputs behavior and callbacks" do
    content = get_module_page([CustomBehaviour])

    assert content =~ %r{<h1>\s*CustomBehaviour\s*<small>behaviour</small>\s*<\/h1>}m
    assert content =~ %r{Callbacks}
    assert content =~ %r{<p class="signature" id="hello/1">}
  end

  ## RECORDS

  test "module_page outputs the record type" do
    content = get_module_page([CompiledRecord])
    assert content =~ %r{<h1>\s*CompiledRecord\s*<small>record</small>\s*<\/h1>}m
  end

  test "module_page outputs record fields" do
    content = get_module_page([CompiledRecord])
    assert content =~ %r{<strong>foo:</strong> nil}m
    assert content =~ %r{<strong>bar:</strong> "sample"}m
  end

  test "module_page outputs exceptions fields" do
    content = get_module_page([RandomError])
    refute content =~ %r{<strong>__exception__:</strong>}m
    assert content =~ %r{<strong>message:</strong> "this is random!"}m
  end

  ## PROTOCOLS

  test "module_page outputs the protocol type" do
    content = get_module_page([CustomProtocol])
    assert content =~ %r{<h1>\s*CustomProtocol\s*<small>protocol</small>\s*<\/h1>}m
  end
end
