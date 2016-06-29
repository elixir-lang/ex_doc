defmodule ExDoc.Formatter.HTML.TemplatesTest do
  use ExUnit.Case, async: true

  alias ExDoc.Formatter.HTML
  alias ExDoc.Formatter.HTML.Templates

  defp source_url do
    "https://github.com/elixir-lang/elixir"
  end

  defp homepage_url do
    "http://elixir-lang.org"
  end

  defp doc_config do
    %ExDoc.Config{
      project: "Elixir",
      version: "1.0.1",
      source_root: File.cwd!,
      source_url_pattern: "#{source_url()}/blob/master/%{path}#L%{line}",
      homepage_url: homepage_url(),
      source_url: source_url()
    }
  end

  defp get_module_page(names) do
    mods =
      names
      |> ExDoc.Retriever.docs_from_modules(doc_config())
      |> HTML.Autolink.all()

    Templates.module_page(hd(mods), [], [], [], doc_config())
  end

  test "synopsis" do
    assert Templates.synopsis(nil) == nil
    assert Templates.synopsis("") == ""
    assert Templates.synopsis(".") == ""
    assert Templates.synopsis(".::.") == ""
    assert Templates.synopsis(" .= .: :.") == ".="
    assert Templates.synopsis(" Description: ") == "Description"
    assert Templates.synopsis("abcd") == "abcd"
    assert_raise FunctionClauseError, fn ->
      Templates.synopsis(:abcd)
    end
  end

  test "synopsis should not end have trailing periods or semicolons" do
    doc1 = """
    Summaries should not be displayed with trailing punctuation . :

    ## Example
    """

    doc2 = """
    Example function: Summary should not display trailing puntuation :.

    ## Example:
    """

    assert Templates.synopsis(doc1) ==
      "Summaries should not be displayed with trailing punctuation"

    assert Templates.synopsis(doc2) ==
      "Example function: Summary should not display trailing puntuation"
  end

  ## LISTING

  test "enables nav link when module type have at least one element" do
    names   = [CompiledWithDocs, CompiledWithDocs.Nested]
    nodes   = ExDoc.Retriever.docs_from_modules(names, doc_config())
    modules = HTML.Autolink.all(nodes)

    content = Templates.sidebar_template(doc_config(), modules, [], [])
    assert content =~ ~r{<li><a id="modules-list" href="#full-list">Modules</a></li>}
    refute content =~ ~r{<li><a id="exceptions-list" href="#full-list">Exceptions</a></li>}
    refute content =~ ~r{<li><a id="protocols-list" href="#full-list">Protocols</a></li>}
  end

  test "site title text links to homepage_url when set" do
    content = Templates.sidebar_template(doc_config(), [], [], [])
    assert content =~ ~r{<a href="#{homepage_url()}" class="sidebar-projectLink">\n\s*<div class="sidebar-projectDetails">\n\s*<h1 class="sidebar-projectName">\n\s*Elixir\n\s*</h1>\n\s*<h2 class="sidebar-projectVersion">\n\s*v1.0.1\n\s*</h2>\n\s*</div>\n\s*</a>}
  end

  test "site title text links to main when there is no homepage_url" do
    config = %ExDoc.Config{project: "Elixir", version: "1.0.1",
                           source_root: File.cwd!, main: "hello",}
    content = Templates.sidebar_template(config, [], [], [])
    assert content =~ ~r{<a href="hello.html" class="sidebar-projectLink">\n\s*<div class="sidebar-projectDetails">\n\s*<h1 class="sidebar-projectName">\n\s*Elixir\n\s*</h1>\n\s*<h2 class="sidebar-projectVersion">\n\s*v1.0.1\n\s*</h2>\n\s*</div>\n\s*</a>}
  end

  test "list_page outputs listing for the given nodes" do
    names = [CompiledWithDocs, CompiledWithDocs.Nested]
    nodes = ExDoc.Retriever.docs_from_modules(names, doc_config())
    content = Templates.create_sidebar_items(%{modules: nodes})

    assert content =~ ~r("modules":\[\{"id":"CompiledWithDocs","title":"CompiledWithDocs")ms
    assert content =~ ~r("id":"CompiledWithDocs".*"functions":.*"example/2")ms
    assert content =~ ~r("id":"CompiledWithDocs".*"macros":.*"example_1/0")ms
    assert content =~ ~r("id":"CompiledWithDocs".*"functions":.*"example_without_docs/0")ms
    assert content =~ ~r("id":"CompiledWithDocs.Nested")ms
  end

  ## MODULES

  test "module_page outputs the functions and docstrings" do
    content = get_module_page([CompiledWithDocs])

    assert content =~ ~r{<title>CompiledWithDocs [^<]*</title>}
    assert content =~ ~r{<h1>\n\s*<small class="visible-xs">Elixir v1.0.1</small>\n\s*CompiledWithDocs\s*}
    refute content =~ ~r{<small>module</small>}
    assert content =~ ~r{moduledoc.*Example.*CompiledWithDocs\.example.*}ms
    assert content =~ ~r{<h2 id="module-example-unicode" class="section-heading">.*<a class="hover-link" href="#module-example-unicode">.*<i class="icon-link"></i>.*</a>.*Example.*</h2>}ms
    assert content =~ ~r{example/2.*Some example}ms
    assert content =~ ~r{example_without_docs/0.*<section class="docstring">.*</section>}ms
    assert content =~ ~r{example_1/0.*Another example}ms
    assert content =~ ~r{<a href="#{source_url()}/blob/master/test/fixtures/compiled_with_docs.ex#L10"[^>]*>\n\s*<i class="icon-code"></i>\n\s*</a>}ms

    assert content =~ ~s{<div class="detail" id="example_1/0">}
    assert content =~ ~s{example(foo, bar \\\\ Baz)}
    assert content =~ ~r{<a href="#example/2" class="detail-link" title="Link to this function">\n\s*<i class="icon-link"><\/i>\n\s*<\/a>}ms
  end

  test "header id generation" do
    assert Templates.header_to_id("“Stale”") == "stale"
    assert Templates.header_to_id("José") == "josé"
    assert Templates.header_to_id(" a - b ") == "a-b"
    assert Templates.header_to_id(" ☃ ") == ""
  end

  test "module_page outputs the types and function specs" do
    content = get_module_page([TypesAndSpecs, TypesAndSpecs.Sub])

    mb = "http://elixir-lang.org/docs/stable"

    public_html =
      ~S[<a href="#t:public/1">public(t)</a> :: {t, ] <>
      ~s[<a href="#{mb}/elixir/String.html#t:t/0">String.t</a>, ] <>
      ~S[<a href="TypesAndSpecs.Sub.html#t:t/0">TypesAndSpecs.Sub.t</a>, ] <>
      ~S[<a href="#t:opaque/0">opaque</a>, :ok | :error}]

    ref_html = ~S[<a href="#t:ref/0">ref</a> :: ] <>
               ~S[{:binary.part, <a href="#t:public/1">public(any)</a>}]

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
    assert content =~ ~r{<div class="summary-signature">\s*<a href="#example_1/0">}
  end

  test "module_page contains links to summary sections when those exist" do
    content = get_module_page([CompiledWithDocs, CompiledWithDocs.Nested])
    refute content =~ ~r{types}
  end

  ## BEHAVIOURS

  test "module_page outputs behavior and callbacks" do
    content = get_module_page([CustomBehaviourOne])
    assert content =~ ~r{<h1>\n\s*<small class="visible-xs">Elixir v1.0.1</small>\n\s*CustomBehaviourOne\s*<small>behaviour</small>}m
    assert content =~ ~r{Callbacks}
    assert content =~ ~r{<div class="detail" id="c:hello/1">}
    assert content =~ ~s[hello(integer)]
    assert content =~ ~s[greet(arg0)]

    content = get_module_page([CustomBehaviourTwo])
    assert content =~ ~r{<h1>\n\s*<small class="visible-xs">Elixir v1.0.1</small>\n\s*CustomBehaviourTwo\s*<small>behaviour</small>\s*}m
    assert content =~ ~r{Callbacks}
    assert content =~ ~r{<div class="detail" id="c:bye/1">}
  end

  ## PROTOCOLS

  test "module_page outputs the protocol type" do
    content = get_module_page([CustomProtocol])
    assert content =~ ~r{<h1>\n\s*<small class="visible-xs">Elixir v1.0.1</small>\n\s*CustomProtocol\s*<small>protocol</small>\s*}m
  end
end
