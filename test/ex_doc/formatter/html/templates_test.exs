defmodule ExDoc.Formatter.HTML.TemplatesTest do
  use ExUnit.Case, async: true

  alias ExDoc.Formatter.HTML
  alias ExDoc.Formatter.HTML.Templates

  @empty_nodes_map %{modules: [], exceptions: [], protocols: [], tasks: []}

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
      source_url: source_url(),
      output: "test/tmp/html_templates"
    }
  end

  defp get_module_page(names) do
    mods =
      names
      |> ExDoc.Retriever.docs_from_modules(doc_config())
      |> HTML.Autolink.all(".html", [])

    Templates.module_page(hd(mods), @empty_nodes_map, doc_config())
  end

  setup_all do
    File.mkdir_p!("test/tmp/html_templates")
    File.cp_r!("priv/ex_doc/formatter/html/assets", "test/tmp/html_templates")
    File.touch!("test/tmp/html_templates/dist/sidebar_items-123456.js")
    :ok
  end

  test "header id generation" do
    assert Templates.header_to_id("“Stale”") == "stale"
    assert Templates.header_to_id("José") == "josé"
    assert Templates.header_to_id(" a - b ") == "a-b"
    assert Templates.header_to_id(" ☃ ") == ""
    assert Templates.header_to_id(" &sup2; ") == ""
    assert Templates.header_to_id(" &#9180; ") == ""
    assert Templates.header_to_id("Git Options (<code class=\"inline\">:git</code>)") == "git-options-git"
  end

  test "link headers" do
    assert Templates.link_headings("<h2>Foo</h2><h2>Bar</h2>") == """
    <h2 id="foo" class="section-heading">
      <a href="#foo" class="hover-link"><span class="icon-link" aria-hidden="true"></span></a>
      Foo
    </h2>
    <h2 id="bar" class="section-heading">
      <a href="#bar" class="hover-link"><span class="icon-link" aria-hidden="true"></span></a>
      Bar
    </h2>
    """

    assert Templates.link_headings("<h2>Foo</h2>\n<h2>Bar</h2>") == """
    <h2 id="foo" class="section-heading">
      <a href="#foo" class="hover-link"><span class="icon-link" aria-hidden="true"></span></a>
      Foo
    </h2>

    <h2 id="bar" class="section-heading">
      <a href="#bar" class="hover-link"><span class="icon-link" aria-hidden="true"></span></a>
      Bar
    </h2>
    """

    assert Templates.link_headings("<h2></h2><h2>Bar</h2>") == """
    <h2></h2><h2 id="bar" class="section-heading">
      <a href="#bar" class="hover-link"><span class="icon-link" aria-hidden="true"></span></a>
      Bar
    </h2>
    """

    assert Templates.link_headings("<h2></h2>\n<h2>Bar</h2>") == """
    <h2></h2>
    <h2 id="bar" class="section-heading">
      <a href="#bar" class="hover-link"><span class="icon-link" aria-hidden="true"></span></a>
      Bar
    </h2>
    """

    assert Templates.link_headings("<h2>Foo</h2><h2></h2>") == String.trim_trailing("""
    <h2 id="foo" class="section-heading">
      <a href="#foo" class="hover-link"><span class="icon-link" aria-hidden="true"></span></a>
      Foo
    </h2>
    <h2></h2>
    """)

    assert Templates.link_headings("<h2>Foo</h2>\n<h2></h2>") == String.trim_trailing("""
    <h2 id="foo" class="section-heading">
      <a href="#foo" class="hover-link"><span class="icon-link" aria-hidden="true"></span></a>
      Foo
    </h2>

    <h2></h2>
    """)

    assert Templates.link_headings("<h3>Foo</h3>") == """
    <h3 id="foo" class="section-heading">
      <a href="#foo" class="hover-link"><span class="icon-link" aria-hidden="true"></span></a>
      Foo
    </h3>
    """
  end

  test "sidebar items from headers" do
    item = %{content: nil, group: nil, id: nil, title: nil}
    assert Templates.create_sidebar_items(%{}, [%{item | content: "<h2>Foo</h2><h2>Bar</h2>"}]) ==
      ~s(sidebarNodes={"extras":[{"id":"","title":"","group":"","headers":[{"id":"Foo","anchor":"foo"},{"id":"Bar","anchor":"bar"}]}]})

    assert Templates.create_sidebar_items(%{}, [%{item | content: "<h2>Foo</h2>\n<h2>Bar</h2>"}]) ==
      ~s(sidebarNodes={"extras":[{"id":"","title":"","group":"","headers":[{"id":"Foo","anchor":"foo"},{"id":"Bar","anchor":"bar"}]}]})

    assert Templates.create_sidebar_items(%{}, [%{item | content: "<h2></h2><h2>Bar</h2>"}]) ==
      ~s(sidebarNodes={"extras":[{"id":"","title":"","group":"","headers":[{"id":"Bar","anchor":"bar"}]}]})

    assert Templates.create_sidebar_items(%{}, [%{item | content: "<h2></h2>\n<h2>Bar</h2>"}]) ==
      ~s(sidebarNodes={"extras":[{"id":"","title":"","group":"","headers":[{"id":"Bar","anchor":"bar"}]}]})

    assert Templates.create_sidebar_items(%{}, [%{item | content: "<h2>Foo</h2><h2></h2>"}]) ==
      ~s(sidebarNodes={"extras":[{"id":"","title":"","group":"","headers":[{"id":"Foo","anchor":"foo"}]}]})

    assert Templates.create_sidebar_items(%{}, [%{item | content: "<h2>Foo</h2>\n<h2></h2>"}]) ==
      ~s(sidebarNodes={"extras":[{"id":"","title":"","group":"","headers":[{"id":"Foo","anchor":"foo"}]}]})
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

  test "<h3> tags in method `@doc`s are linked" do
    content = get_module_page([CompiledWithDocs])
    assert content =~ ~r{<h3 id="example_with_h3/0-examples" class="section-heading">.*<a href="#example_with_h3/0-examples" class="hover-link">.*<span class="icon-link" aria-hidden="true"></span>.*</a>.*Examples.*</h3>}ms
  end

  ## LISTING

  test "site title text links to homepage_url when set" do
    content = Templates.sidebar_template(doc_config(), @empty_nodes_map)
    assert content =~ ~r{<a href="#{homepage_url()}" class="sidebar-projectLink">\s*<div class="sidebar-projectDetails">\s*<h1 class="sidebar-projectName">\s*Elixir\s*</h1>\s*<h2 class="sidebar-projectVersion">\s*v1.0.1\s*</h2>\s*</div>\s*</a>}
  end

  test "site title text links to main when there is no homepage_url" do
    config = %ExDoc.Config{project: "Elixir", version: "1.0.1",
                           source_root: File.cwd!, main: "hello",}
    content = Templates.sidebar_template(config, @empty_nodes_map)
    assert content =~ ~r{<a href="hello.html" class="sidebar-projectLink">\s*<div class="sidebar-projectDetails">\s*<h1 class="sidebar-projectName">\s*Elixir\s*</h1>\s*<h2 class="sidebar-projectVersion">\s*v1.0.1\s*</h2>\s*</div>\s*</a>}
  end

  test "list_page enables nav link when module type have at least one element" do
    names   = [CompiledWithDocs, CompiledWithDocs.Nested]
    nodes   = ExDoc.Retriever.docs_from_modules(names, doc_config())
    modules = HTML.Autolink.all(nodes, ".html", [])

    content = Templates.sidebar_template(doc_config(), %{modules: modules, exceptions: [], protocols: [], tasks: []})
    assert content =~ ~r{<li><a id="modules-list" href="#full-list">Modules</a></li>}
    refute content =~ ~r{<li><a id="exceptions-list" href="#full-list">Exceptions</a></li>}
    refute content =~ ~r{<li><a id="protocols-list" href="#full-list">Protocols</a></li>}
  end

  test "list_page outputs listing for the given nodes" do
    names = [CompiledWithDocs, CompiledWithDocs.Nested]
    nodes = ExDoc.Retriever.docs_from_modules(names, doc_config())
    content = Templates.create_sidebar_items(%{modules: nodes}, [])

    assert content =~ ~r("modules":\[\{"id":"CompiledWithDocs","title":"CompiledWithDocs")ms
    assert content =~ ~r("id":"CompiledWithDocs".*"functions":.*"example/2")ms
    assert content =~ ~r("id":"CompiledWithDocs".*"functions":.*"example_without_docs/0")ms
    assert content =~ ~r("id":"CompiledWithDocs.Nested")ms
  end

  ## MODULES

  test "module_page outputs the functions and docstrings" do
    content = get_module_page([CompiledWithDocs])

    # Title and headers
    assert content =~ ~r{<title>CompiledWithDocs [^<]*</title>}
    assert content =~ ~r{<h1>\s*<small class="visible-xs">Elixir v1.0.1</small>\s*CompiledWithDocs\s*}
    refute content =~ ~r{<small>module</small>}
    assert content =~ ~r{moduledoc.*Example.*CompiledWithDocs\.example.*}ms
    assert content =~ ~r{<h2 id="module-example-unicode-escaping" class="section-heading">.*<a href="#module-example-unicode-escaping" class="hover-link">.*<span class="icon-link" aria-hidden="true"></span>.*</a>.*Example.*</h2>}ms
    assert content =~ ~r{<h3 id="module-example-h3-heading" class="section-heading">.*<a href="#module-example-h3-heading" class="hover-link">.*<span class="icon-link" aria-hidden="true"></span>.*</a>.*Example H3 heading.*</h3>}ms

    # Summaries
    assert content =~ ~r{example/2.*Some example}ms
    assert content =~ ~r{example_without_docs/0.*<section class="docstring">.*</section>}ms
    assert content =~ ~r{example_1/0.*<span class="note">\(macro\)</span>}ms

    # Source
    assert content =~ ~r{<a href="#{source_url()}/blob/master/test/fixtures/compiled_with_docs.ex#L14"[^>]*>\s*<span class="icon-code" aria-hidden="true"></span>\s*<span class="sr-only">View Source</span>\s*</a>}ms

    # Functions
    assert content =~ ~s{<div class="detail" id="example/2">}
    assert content =~ ~s{<span id="example/1"></span>}
    assert content =~ ~s{example(foo, bar \\\\ Baz)}
    assert content =~ ~r{<a href="#example/2" class="detail-link" title="Link to this function">\s*<span class="icon-link" aria-hidden="true"></span>\s*<span class="sr-only">Link to this function</span>\s*</a>}ms
  end

  test "module_page outputs the types and function specs" do
    content = get_module_page([TypesAndSpecs, TypesAndSpecs.Sub])

    public_html =
      ~S[public(t) :: {t, ] <>
      ~s[<a href="https://hexdocs.pm/elixir/String.html#t:t/0">String.t</a>, ] <>
      ~S[<a href="TypesAndSpecs.Sub.html#t:t/0">TypesAndSpecs.Sub.t</a>, ] <>
      ~S[<a href="#t:opaque/0">opaque</a>, :ok | :error}]

    ref_html = ~S[ref() :: {:binary.part, <a href="#t:public/1">public</a>(any)}]

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
    assert content =~ ~r{<h1>\s*<small class="visible-xs">Elixir v1.0.1</small>\s*CustomBehaviourOne\s*<small>behaviour</small>}m
    assert content =~ ~r{Callbacks}
    assert content =~ ~r{<div class="detail" id="c:hello/1">}
    assert content =~ ~s[hello(integer)]
    assert content =~ ~s[greet(arg0)]

    content = get_module_page([CustomBehaviourTwo])
    assert content =~ ~r{<h1>\s*<small class="visible-xs">Elixir v1.0.1</small>\s*CustomBehaviourTwo\s*<small>behaviour</small>\s*}m
    assert content =~ ~r{Callbacks}
    assert content =~ ~r{<div class="detail" id="c:bye/1">}
  end

  ## PROTOCOLS

  test "module_page outputs the protocol type" do
    content = get_module_page([CustomProtocol])
    assert content =~ ~r{<h1>\s*<small class="visible-xs">Elixir v1.0.1</small>\s*CustomProtocol\s*<small>protocol</small>\s*}m
  end

  ## TASKS

  test "module_page outputs the task type" do
    content = get_module_page([Mix.Tasks.TaskWithDocs])
    assert content =~ ~r{<h1>\s*<small class="visible-xs">Elixir v1.0.1</small>\s*mix task_with_docs\s*}m
  end
end
