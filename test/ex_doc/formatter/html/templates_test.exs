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

  defp doc_config(config \\ []) do
    default = %ExDoc.Config{
      project: "Elixir",
      version: "1.0.1",
      source_root: File.cwd!(),
      source_url_pattern: "#{source_url()}/blob/master/%{path}#L%{line}",
      homepage_url: homepage_url(),
      source_url: source_url(),
      output: "test/tmp/html_templates"
    }

    struct(default, config)
  end

  defp get_module_page(names, config \\ []) do
    config = doc_config(config)
    mods = ExDoc.Retriever.docs_from_modules(names, config)
    [mod | _] = HTML.render_all(mods, ".html", config, [])
    Templates.module_page(mod, @empty_nodes_map, config)
  end

  setup_all do
    File.mkdir_p!("test/tmp/html_templates")
    File.cp_r!("formatters/html", "test/tmp/html_templates")
    File.touch!("test/tmp/html_templates/dist/sidebar_items-123456.js")
    File.touch!("test/tmp/html_templates/dist/search_items-123456.js")
    :ok
  end

  describe "link_headings" do
    test "generates headers with hovers" do
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

      assert Templates.link_headings("<h2>Foo</h2><h2></h2>") ==
               String.trim_trailing("""
               <h2 id="foo" class="section-heading">
                 <a href="#foo" class="hover-link"><span class="icon-link" aria-hidden="true"></span></a>
                 Foo
               </h2>
               <h2></h2>
               """)

      assert Templates.link_headings("<h2>Foo</h2>\n<h2></h2>") ==
               String.trim_trailing("""
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

    test "generates headers with unique id's" do
      assert Templates.link_headings("<h3>Foo</h3>\n<h3>Foo</h3>") == """
             <h3 id="foo" class="section-heading">
               <a href="#foo" class="hover-link"><span class="icon-link" aria-hidden="true"></span></a>
               Foo
             </h3>

             <h3 id="foo-1" class="section-heading">
               <a href="#foo-1" class="hover-link"><span class="icon-link" aria-hidden="true"></span></a>
               Foo
             </h3>
             """
    end
  end

  describe "synopsis" do
    test "functionality" do
      assert Templates.synopsis(nil) == nil
      assert Templates.synopsis("") == ""
      assert Templates.synopsis("<p>.</p>") == "<p>.</p>"
      assert Templates.synopsis("<p>::</p>") == "<p></p>"
      assert Templates.synopsis("<p>Description:</p>") == "<p>Description</p>"
      assert Templates.synopsis("<p>abcd</p>") == "<p>abcd</p>"
    end

    test "should not end have trailing periods or semicolons" do
      doc1 = """
      Summaries should not be displayed with trailing semicolons :

      ## Example
      """

      doc2 = """
      Example function: Summary should display trailing period :.

      ## Example:
      """

      assert Templates.synopsis(to_html(doc1)) ==
               "<p>Summaries should not be displayed with trailing semicolons </p>"

      assert Templates.synopsis(to_html(doc2)) ==
               "<p>Example function: Summary should display trailing period :.</p>"
    end
  end

  defp to_html(markdown) do
    markdown
    |> ExDoc.Markdown.to_ast()
    |> ExDoc.Formatter.HTML.ast_to_html()
    |> IO.iodata_to_binary()
  end

  describe "sidebar" do
    test "text links to homepage_url when set" do
      content = Templates.sidebar_template(doc_config(), @empty_nodes_map)

      assert content =~
               ~r{<div class="sidebar-header">\s*<div class="sidebar-projectDetails">\s*<a href="#{
                 homepage_url()
               }" class="sidebar-projectName">\s*Elixir\s*</a>\s*<h2 class="sidebar-projectVersion">\s*v1.0.1\s*</h2>\s*</div>\s*</div>}
    end

    test "text links to main when there is no homepage_url" do
      config = %ExDoc.Config{
        project: "Elixir",
        version: "1.0.1",
        source_root: File.cwd!(),
        main: "hello"
      }

      content = Templates.sidebar_template(config, @empty_nodes_map)

      assert content =~
               ~r{<div class="sidebar-header">\s*<div class="sidebar-projectDetails">\s*<a href="hello.html" class="sidebar-projectName">\s*Elixir\s*</a>\s*<h2 class="sidebar-projectVersion">\s*v1.0.1\s*</h2>\s*</div>\s*</div>}
    end

    test "enables nav link when module type have at least one element" do
      names = [CompiledWithDocs, CompiledWithDocs.Nested]
      modules = ExDoc.Retriever.docs_from_modules(names, doc_config())

      content =
        Templates.sidebar_template(doc_config(), %{
          modules: modules,
          exceptions: [],
          tasks: []
        })

      assert content =~ ~r{<li><a id="modules-list" href="#full-list">Modules</a></li>}
      refute content =~ ~r{<li><a id="exceptions-list" href="#full-list">Exceptions</a></li>}
      refute content =~ ~r{<li><a id="tasks-list" href="#full-list">Mix Tasks</a></li>}
    end

    test "outputs listing for the given nodes" do
      names = [CompiledWithDocs, CompiledWithDocs.Nested]
      nodes = ExDoc.Retriever.docs_from_modules(names, doc_config())
      content = create_sidebar_items(%{modules: nodes}, [])

      assert content =~ ~r("modules":\[\{.*"id":"CompiledWithDocs",.*"title":"CompiledWithDocs")ms
      assert content =~ ~r("id":"CompiledWithDocs".*"key":"functions".*"example/2")ms
      assert content =~ ~r("id":"CompiledWithDocs".*"key":"functions".*"example_without_docs/0")ms
      assert content =~ ~r("id":"CompiledWithDocs.Nested")ms
    end

    test "outputs nodes grouped based on metadata" do
      nodes =
        ExDoc.Retriever.docs_from_modules(
          [CompiledWithDocs, CompiledWithDocs.Nested],
          doc_config(
            groups_for_functions: [
              "Example functions": &(&1[:purpose] == :example),
              Legacy: &is_binary(&1[:deprecated])
            ]
          )
        )

      content = create_sidebar_items(%{modules: nodes}, [])

      assert content =~
               ~r("modules":\[\{"group":"","id":"CompiledWithDocs",.*"title":"CompiledWithDocs")ms

      assert content =~ ~r("key":"example-functions".*"example/2")ms
      refute content =~ ~r("key":"legacy".*"example/2")ms
      refute content =~ ~r("key":"functions".*"example/2")ms
      assert content =~ ~r("key":"functions".*"example_1/0")ms
      assert content =~ ~r("key":"legacy".*"example_without_docs/0")ms
    end

    test "outputs module groups for the given nodes" do
      names = [CompiledWithDocs, CompiledWithDocs.Nested]
      group_mapping = [groups_for_modules: [Group: [CompiledWithDocs]]]
      nodes = ExDoc.Retriever.docs_from_modules(names, doc_config(group_mapping))
      content = create_sidebar_items(%{modules: nodes}, [])

      assert content =~ ~r("group":"Group","id":"CompiledWithDocs",.*"title":"CompiledWithDocs")ms
    end

    test "outputs extras with headers" do
      item = %{content: nil, group: nil, id: nil, title: nil}

      assert create_sidebar_items(%{}, [%{item | content: "<h2>Foo</h2><h2>Bar</h2>"}]) ==
               ~s(sidebarNodes={"extras":[{"group":"","headers":[{"anchor":"foo","id":"Foo"},{"anchor":"bar","id":"Bar"}],"id":"","title":""}]})

      assert create_sidebar_items(%{}, [%{item | content: "<h2>Foo</h2>\n<h2>Bar</h2>"}]) ==
               ~s(sidebarNodes={"extras":[{"group":"","headers":[{"anchor":"foo","id":"Foo"},{"anchor":"bar","id":"Bar"}],"id":"","title":""}]})

      assert create_sidebar_items(%{}, [%{item | content: "<h2></h2><h2>Bar</h2>"}]) ==
               ~s(sidebarNodes={"extras":[{"group":"","headers":[{"anchor":"bar","id":"Bar"}],"id":"","title":""}]})

      assert create_sidebar_items(%{}, [%{item | content: "<h2>Foo</h2><h2></h2>"}]) ==
               ~s(sidebarNodes={"extras":[{"group":"","headers":[{"anchor":"foo","id":"Foo"}],"id":"","title":""}]})
    end

    test "builds sections out of moduledocs" do
      names = [CompiledWithDocs, CompiledWithoutDocs, DuplicateHeadings]
      config = doc_config()
      nodes = ExDoc.Retriever.docs_from_modules(names, config)
      nodes = HTML.render_all(nodes, ".html", config, [])

      assert "sidebarNodes=" <> json = create_sidebar_items(%{modules: nodes}, [])

      assert {:ok, %{modules: [compiled_with_docs, compiled_without_docs, duplicate_headings]}} =
               Jason.decode(json, keys: :atoms)

      assert compiled_with_docs.sections == [
               %{
                 anchor: "module-example-unicode-escaping",
                 id: "Example ☃ Unicode &gt; escaping"
               }
             ]

      assert compiled_without_docs.sections == []

      assert duplicate_headings.sections == [
               %{anchor: "module-one", id: "One"},
               %{anchor: "module-two", id: "Two"},
               %{anchor: "module-one-1", id: "One"},
               %{anchor: "module-two-1", id: "Two"},
               %{anchor: "module-one-2", id: "One"},
               %{anchor: "module-two-2", id: "Two"}
             ]
    end

    defp create_sidebar_items(nodes_map, extras) do
      nodes_map
      |> Templates.create_sidebar_items(extras)
      |> IO.iodata_to_binary()
    end
  end

  describe "module_page" do
    test "outputs the functions and docstrings" do
      content = get_module_page([CompiledWithDocs])

      # Title and headers
      assert content =~ ~r{<title>CompiledWithDocs [^<]*</title>}

      assert content =~
               ~r{<h1>\s*<small class="app-vsn">Elixir v1.0.1</small>\s*CompiledWithDocs\s*}

      refute content =~ ~r{<small>module</small>}

      assert content =~
               ~r{moduledoc.*Example.*<span class="nc">CompiledWithDocs</span><span class="o">\.</span><span class="n">example</span>.*}ms

      assert content =~
               ~r{<h2 id="module-example-unicode-escaping" class="section-heading">.*<a href="#module-example-unicode-escaping" class="hover-link">.*<span class="icon-link" aria-hidden="true"></span>.*</a>.*Example.*</h2>}ms

      assert content =~
               ~r{<h3 id="module-example-h3-heading" class="section-heading">.*<a href="#module-example-h3-heading" class="hover-link">.*<span class="icon-link" aria-hidden="true"></span>.*</a>.*Example H3 heading.*</h3>}ms

      # Summaries
      assert content =~ ~r{example/2.*Some example}ms
      assert content =~ ~r{example_without_docs/0.*<section class="docstring">.*</section>}ms
      assert content =~ ~r{example_1/0.*<span class="note">\(macro\)</span>}ms

      # Source
      assert content =~
               ~r{<a href="#{source_url()}/blob/master/test/fixtures/compiled_with_docs.ex#L14"[^>]*>\s*<span class="icon-code" aria-hidden="true"></span>\s*<span class="sr-only">View Source</span>\s*</a>}ms

      # Functions
      assert content =~ ~s{<section class="detail" id="example/2">}
      assert content =~ ~s{<span id="example/1"></span>}
      assert content =~ ~s{example(foo, bar \\\\ Baz)}

      assert content =~
               ~r{<a href="#example/2" class="detail-link" title="Link to this function">\s*<span class="icon-link" aria-hidden="true"></span>\s*<span class="sr-only">Link to this function</span>\s*</a>}ms
    end

    test "outputs function groups" do
      content =
        get_module_page([CompiledWithDocs],
          groups_for_functions: [
            "Example functions": &(&1[:purpose] == :example),
            Legacy: &is_binary(&1[:deprecated])
          ]
        )

      assert content =~ ~r{id="example-functions".*href="#example-functions".*Example functions}ms
      assert content =~ ~r{id="legacy".*href="#legacy".*Legacy}ms
      assert content =~ ~r{id="example-functions".*id="example/2"}ms
      refute content =~ ~r{id="legacy".*id="example/2"}ms
      refute content =~ ~r{id="functions".*id="example/2"}ms
      assert content =~ ~r{id="functions".*id="example_1/0"}ms
    end

    test "outputs deprecation information" do
      content = get_module_page([CompiledWithDocs])

      assert content =~
               ~s{<span class="deprecated" title="Use something else instead">deprecated</span>}

      assert content =~
               ~r{<div class="deprecated">\s*This function is deprecated. Use something else instead.}
    end

    test "outputs the types and function specs" do
      content =
        get_module_page([TypesAndSpecs, TypesAndSpecs.Sub],
          skip_undefined_reference_warnings_on: ["t:TypesAndSpecs.private/0"]
        )

      integer = ~s[<a href="https://hexdocs.pm/elixir/typespecs.html#basic-types">integer</a>()]

      public_html =
        ~S[public(t) :: {t, ] <>
          ~s[<a href="https://hexdocs.pm/elixir/String.html#t:t/0">String.t</a>(), ] <>
          ~S[<a href="TypesAndSpecs.Sub.html#t:t/0">TypesAndSpecs.Sub.t</a>(), ] <>
          ~S[<a href="#t:opaque/0">opaque</a>(), :ok | :error}]

      assert content =~ ~s[<a href="#t:public/1">public(t)</a>]
      refute content =~ ~s[<a href="#t:private/0">private</a>]
      assert content =~ public_html
      refute content =~ ~s[<strong>private\(t\)]
      assert content =~ ~s[A public type]
      assert content =~ ~s[add(#{integer}, <a href="#t:opaque/0">opaque</a>()) :: #{integer}]
      refute content =~ ~s[minus(#{integer}, #{integer}) :: #{integer}]

      assert content =~
               ~s[Basic type: <a href=\"https://hexdocs.pm/elixir/typespecs.html#basic-types\"><code class=\"inline\">atom/0</code></a>.]

      assert content =~ ~r{opaque/0.*<span class="note">\(opaque\)</span>}ms
    end

    test "outputs summaries" do
      content = get_module_page([CompiledWithDocs])
      assert content =~ ~r{<div class="summary-signature">\s*<a href="#example_1/0">}
    end

    test "contains links to summary sections when those exist" do
      content = get_module_page([CompiledWithDocs, CompiledWithDocs.Nested])
      refute content =~ ~r{types}
    end

    test "add hovers to <h3> tags" do
      content = get_module_page([CompiledWithDocs])

      assert content =~
               ~r{<h3 id="example_with_h3/0-examples" class="section-heading">.*<a href="#example_with_h3/0-examples" class="hover-link">.*<span class="icon-link" aria-hidden="true"></span>.*</a>.*Examples.*</h3>}ms
    end

    ## BEHAVIOURS

    test "outputs behavior and callbacks" do
      content = get_module_page([CustomBehaviourOne])

      assert content =~
               ~r{<h1>\s*<small class="app-vsn">Elixir v1.0.1</small>\s*CustomBehaviourOne\s*<small>behaviour</small>}m

      assert content =~ ~r{Callbacks}
      assert content =~ ~r{<section class="detail" id="c:hello/1">}
      assert content =~ ~s[hello(%URI{})]
      assert content =~ ~s[greet(arg1)]

      content = get_module_page([CustomBehaviourTwo])

      assert content =~
               ~r{<h1>\s*<small class="app-vsn">Elixir v1.0.1</small>\s*CustomBehaviourTwo\s*<small>behaviour</small>\s*}m

      assert content =~ ~r{Callbacks}
      assert content =~ ~r{<section class="detail" id="c:bye/1">}
    end

    ## PROTOCOLS

    test "outputs the protocol type" do
      content = get_module_page([CustomProtocol])

      assert content =~
               ~r{<h1>\s*<small class="app-vsn">Elixir v1.0.1</small>\s*CustomProtocol\s*<small>protocol</small>\s*}m
    end

    ## TASKS

    test "outputs the task type" do
      content = get_module_page([Mix.Tasks.TaskWithDocs])

      assert content =~
               ~r{<h1>\s*<small class="app-vsn">Elixir v1.0.1</small>\s*mix task_with_docs\s*}m
    end
  end
end
