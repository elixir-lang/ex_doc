defmodule ExDoc.Formatter.HTML.TemplatesTest do
  use ExUnit.Case, async: true

  alias ExDoc.Formatter.HTML
  alias ExDoc.Formatter.HTML.Templates

  @moduletag :tmp_dir

  @empty_nodes_map %{modules: [], exceptions: [], protocols: [], tasks: []}

  defp source_url do
    "https://github.com/elixir-lang/elixir"
  end

  defp homepage_url do
    "https://elixir-lang.org"
  end

  defp doc_config(context, config \\ []) do
    default = %ExDoc.Config{
      project: "Elixir",
      version: "1.0.1",
      source_url_pattern: "#{source_url()}/blob/master/%{path}#L%{line}",
      homepage_url: homepage_url(),
      source_url: source_url(),
      output: context.tmp_dir <> "/html_templates"
    }

    struct(default, config)
  end

  defp get_module_page(names, context, config \\ []) do
    config = doc_config(context, config)
    {mods, []} = ExDoc.Retriever.docs_from_modules(names, config)
    [mod | _] = HTML.render_all(mods, [], ".html", config, [])
    Templates.module_page(mod, @empty_nodes_map, config)
  end

  setup %{tmp_dir: tmp_dir} do
    File.cp_r!("formatters/html", tmp_dir <> "/html_templates")
    File.touch!(tmp_dir <> "/html_templates/dist/sidebar_items-123456.js")
    File.touch!(tmp_dir <> "/html_templates/dist/search_data-123456.js")
    :ok
  end

  describe "link_headings" do
    test "generates headers with hovers" do
      assert Templates.link_headings("<h2>Foo</h2><h2>Bar</h2>") == """
             <h2 id="foo" class="section-heading">
               <a href="#foo" class="hover-link">
                 <i class="ri-link-m" aria-hidden="true"></i>
               </a>
               <span class="text">Foo</span>
             </h2>
             <h2 id="bar" class="section-heading">
               <a href="#bar" class="hover-link">
                 <i class="ri-link-m" aria-hidden="true"></i>
               </a>
               <span class="text">Bar</span>
             </h2>
             """

      assert Templates.link_headings("<h2>Foo</h2>\n<h2>Bar</h2>") == """
             <h2 id="foo" class="section-heading">
               <a href="#foo" class="hover-link">
                 <i class="ri-link-m" aria-hidden="true"></i>
               </a>
               <span class="text">Foo</span>
             </h2>

             <h2 id="bar" class="section-heading">
               <a href="#bar" class="hover-link">
                 <i class="ri-link-m" aria-hidden="true"></i>
               </a>
               <span class="text">Bar</span>
             </h2>
             """

      assert Templates.link_headings("<h2></h2><h2>Bar</h2>") == """
             <h2></h2><h2 id="bar" class="section-heading">
               <a href="#bar" class="hover-link">
                 <i class="ri-link-m" aria-hidden="true"></i>
               </a>
               <span class="text">Bar</span>
             </h2>
             """

      assert Templates.link_headings("<h2></h2>\n<h2>Bar</h2>") == """
             <h2></h2>
             <h2 id="bar" class="section-heading">
               <a href="#bar" class="hover-link">
                 <i class="ri-link-m" aria-hidden="true"></i>
               </a>
               <span class="text">Bar</span>
             </h2>
             """

      assert Templates.link_headings("<h2>Foo</h2><h2></h2>") ==
               String.trim_trailing("""
               <h2 id="foo" class="section-heading">
                 <a href="#foo" class="hover-link">
                   <i class="ri-link-m" aria-hidden="true"></i>
                 </a>
                 <span class="text">Foo</span>
               </h2>
               <h2></h2>
               """)

      assert Templates.link_headings("<h2>Foo</h2>\n<h2></h2>") ==
               String.trim_trailing("""
               <h2 id="foo" class="section-heading">
                 <a href="#foo" class="hover-link">
                   <i class="ri-link-m" aria-hidden="true"></i>
                 </a>
                 <span class="text">Foo</span>
               </h2>

               <h2></h2>
               """)

      assert Templates.link_headings("<h3>Foo</h3>") == """
             <h3 id="foo" class="section-heading">
               <a href="#foo" class="hover-link">
                 <i class="ri-link-m" aria-hidden="true"></i>
               </a>
               <span class="text">Foo</span>
             </h3>
             """
    end

    test "generates headers with unique id's" do
      assert Templates.link_headings("<h3>Foo</h3>\n<h3>Foo</h3>") == """
             <h3 id="foo" class="section-heading">
               <a href="#foo" class="hover-link">
                 <i class="ri-link-m" aria-hidden="true"></i>
               </a>
               <span class="text">Foo</span>
             </h3>

             <h3 id="foo-1" class="section-heading">
               <a href="#foo-1" class="hover-link">
                 <i class="ri-link-m" aria-hidden="true"></i>
               </a>
               <span class="text">Foo</span>
             </h3>
             """
    end

    test "generates headers for admonition support" do
      admonition_block = """
      <blockquote><h3 class="warning">Foo</h3></blockquote>
      """

      assert Templates.link_headings(admonition_block) == """
             <blockquote><h3 id="foo" class="warning section-heading">
               <a href="#foo" class="hover-link">
                 <i class="ri-link-m" aria-hidden="true"></i>
               </a>
               <span class="text">Foo</span>
             </h3>
             </blockquote>
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
    |> ExDoc.DocAST.parse!("text/markdown")
    |> ExDoc.DocAST.to_string()
  end

  describe "sidebar" do
    test "text links to homepage_url when set", context do
      content = Templates.sidebar_template(doc_config(context), @empty_nodes_map)

      assert content =~
               ~r"""
               <div class="sidebar-header">\s*\
               <div class="sidebar-projectInfo">\s*\
               <div>\s*\
               <a href="#{homepage_url()}" class="sidebar-projectName" translate="no">\s*Elixir\s*</a>\s*\
               <div class="sidebar-projectVersion" translate="no">\s*v1.0.1\s*</div>\s*\</div>\s*\</div>
               """
    end

    test "text links to main when there is no homepage_url" do
      config = %ExDoc.Config{
        project: "Elixir",
        version: "1.0.1",
        main: "hello"
      }

      content = Templates.sidebar_template(config, @empty_nodes_map)

      assert content =~
               ~r"""
               <div class="sidebar-header">\s*\
               <div class="sidebar-projectInfo">\s*\
               <div>\s*\
               <a href="hello.html" class="sidebar-projectName" translate="no">\s*Elixir\s*</a>\s*\
               <div class="sidebar-projectVersion" translate="no">\s*v1.0.1\s*</div>\s*\</div>\s*\</div>
               """
    end

    test "enables nav link when module type have at least one element", context do
      names = [CompiledWithDocs, CompiledWithDocs.Nested]
      modules = ExDoc.Retriever.docs_from_modules(names, doc_config(context))

      content =
        Templates.sidebar_template(doc_config(context), %{
          modules: modules,
          exceptions: [],
          tasks: []
        })

      assert content =~
               ~r{<li>[\s\n]*<button id="modules-list-tab-button" role="tab" data-type="modules" aria-controls="modules-tab-panel" aria-selected="false" tabindex="-1">[\s\n]*Modules[\s\n]*</button>[\s\n]*</li>}

      assert content =~
               ~r{<div id="modules-tab-panel" class="sidebar-tabpanel" role="tabpanel" aria-labelledby="modules-list-tab-button" hidden>[\n\s]*<ul id="modules-full-list" class="full-list"></ul>[\n\s]*</div>}

      refute content =~ ~r{id="tasks-list-tab-button"}
      refute content =~ ~r{id="tasks-full-list"}
    end

    test "display built with footer by proglang option", context do
      content = Templates.footer_template(doc_config(context, proglang: :erlang), nil)

      assert content =~
               ~r{<a href="https://erlang.org" title="Erlang" target="_blank" translate="no">Erlang programming language</a>}

      content = Templates.footer_template(doc_config(context, proglang: :elixir), nil)

      assert content =~
               ~r{<a href="https://elixir-lang.org" title="Elixir" target="_blank" translate="no">Elixir programming language</a>}

      assert Templates.footer_template(doc_config(context, proglang: :elixir), nil) ==
               Templates.footer_template(doc_config(context), nil)
    end

    test "outputs listing for the given nodes", context do
      names = [CompiledWithDocs, CompiledWithDocs.Nested]
      {nodes, []} = ExDoc.Retriever.docs_from_modules(names, doc_config(context))

      assert [
               %{
                 "id" => "CompiledWithDocs",
                 "title" => "CompiledWithDocs",
                 "deprecated" => false,
                 "nodeGroups" => [
                   %{
                     "key" => "functions",
                     "nodes" => [
                       %{"anchor" => "__struct__/0", "id" => "%CompiledWithDocs{}"},
                       %{"anchor" => "example/2", "id" => "example/2"},
                       %{"anchor" => "example_1/0", "id" => "example_1/0"},
                       %{"anchor" => "example_with_h3/0", "id" => "example_with_h3/0"},
                       %{"anchor" => "example_without_docs/0", "id" => "example_without_docs/0"},
                       %{"anchor" => "flatten/1", "id" => "flatten/1"},
                       %{"anchor" => "is_zero/1", "id" => "is_zero/1"},
                       %{"anchor" => "name/with/slashes/0", "id" => "name/with/slashes/0"}
                     ]
                   }
                 ]
               },
               %{"id" => "CompiledWithDocs.Nested"}
             ] = create_sidebar_items(%{modules: nodes}, [])["modules"]
    end

    test "outputs deprecated: true if node is deprecated", context do
      names = [CompiledWithDocs]
      {nodes, []} = ExDoc.Retriever.docs_from_modules(names, doc_config(context))

      path = ["modules", Access.at!(0), "nodeGroups", Access.at!(0), "nodes"]
      sidebar_functions = get_in(create_sidebar_items(%{modules: nodes}, []), path)

      assert Enum.any?(
               sidebar_functions,
               &match?(%{"anchor" => "example/2", "deprecated" => true}, &1)
             )
    end

    test "outputs deprecated: true if module is deprecated", context do
      names = [Warnings]
      {nodes, []} = ExDoc.Retriever.docs_from_modules(names, doc_config(context))

      assert Enum.any?(
               create_sidebar_items(%{modules: nodes}, [])["modules"],
               &match?(%{"title" => "Warnings", "deprecated" => true}, &1)
             )
    end

    test "outputs nodes grouped based on metadata", context do
      {nodes, []} =
        ExDoc.Retriever.docs_from_modules(
          [CompiledWithDocs, CompiledWithDocs.Nested],
          doc_config(context,
            groups_for_docs: [
              "Example functions": &(&1[:purpose] == :example),
              Legacy: &is_binary(&1[:deprecated])
            ]
          )
        )

      assert [
               %{
                 "group" => "",
                 "id" => "CompiledWithDocs",
                 "nodeGroups" => [
                   %{
                     "key" => "example-functions",
                     "nodes" => [
                       %{"id" => "example/2"},
                       %{"id" => "example_with_h3/0"}
                     ]
                   },
                   %{
                     "key" => "legacy",
                     "nodes" => [%{"id" => "example_without_docs/0"}]
                   },
                   %{
                     "key" => "functions",
                     "nodes" => [
                       %{"id" => "%CompiledWithDocs{}"},
                       %{"id" => "example_1/0"},
                       %{"id" => "flatten/1"},
                       %{"id" => "is_zero/1"},
                       %{"id" => "name/with/slashes/0"}
                     ]
                   }
                 ]
               },
               %{"id" => "CompiledWithDocs.Nested"}
             ] = create_sidebar_items(%{modules: nodes}, [])["modules"]
    end

    test "outputs module groups for the given nodes", context do
      names = [CompiledWithDocs, CompiledWithDocs.Nested]
      group_mapping = [groups_for_modules: [Group: [CompiledWithDocs]]]
      {nodes, []} = ExDoc.Retriever.docs_from_modules(names, doc_config(context, group_mapping))

      assert [
               %{"group" => ""},
               %{
                 "group" => "Group",
                 "id" => "CompiledWithDocs",
                 "title" => "CompiledWithDocs"
               }
             ] = create_sidebar_items(%{modules: nodes}, [])["modules"]
    end

    test "outputs extras with headers" do
      item = %{content: nil, group: nil, id: nil, title: nil}

      assert create_sidebar_items(%{}, [%{item | content: "<h2>Foo</h2><h2>Bar</h2>"}])["extras"] ==
               [
                 %{
                   "group" => "",
                   "headers" => [
                     %{"anchor" => "foo", "id" => "Foo"},
                     %{"anchor" => "bar", "id" => "Bar"}
                   ],
                   "id" => "",
                   "title" => ""
                 }
               ]

      assert create_sidebar_items(%{}, [%{item | content: "<h2>Foo</h2>\n<h2>Bar</h2>"}])[
               "extras"
             ] ==
               [
                 %{
                   "group" => "",
                   "headers" => [
                     %{"anchor" => "foo", "id" => "Foo"},
                     %{"anchor" => "bar", "id" => "Bar"}
                   ],
                   "id" => "",
                   "title" => ""
                 }
               ]

      assert create_sidebar_items(%{}, [%{item | content: "<h2>Foo</h2><h2></h2>"}])["extras"] ==
               [
                 %{
                   "group" => "",
                   "headers" => [
                     %{"anchor" => "foo", "id" => "Foo"}
                   ],
                   "id" => "",
                   "title" => ""
                 }
               ]
    end

    test "builds sections out of moduledocs", context do
      names = [CompiledWithDocs, CompiledWithoutDocs, DuplicateHeadings]
      config = doc_config(context)
      {nodes, []} = ExDoc.Retriever.docs_from_modules(names, config)
      nodes = HTML.render_all(nodes, [], ".html", config, [])

      [compiled_with_docs, compiled_without_docs, duplicate_headings] =
        create_sidebar_items(%{modules: nodes}, [])["modules"]

      assert compiled_with_docs["sections"] == [
               %{
                 "anchor" => "module-example-unicode-escaping",
                 "id" => "Example ☃ Unicode &gt; escaping"
               }
             ]

      assert compiled_without_docs["sections"] == []

      assert duplicate_headings["sections"] == [
               %{"anchor" => "module-one", "id" => "One"},
               %{"anchor" => "module-two", "id" => "Two"},
               %{"anchor" => "module-one-1", "id" => "One"},
               %{"anchor" => "module-two-1", "id" => "Two"},
               %{"anchor" => "module-one-2", "id" => "One"},
               %{"anchor" => "module-two-2", "id" => "Two"}
             ]
    end

    defp create_sidebar_items(nodes_map, extras) do
      "sidebarNodes=" <> content =
        nodes_map
        |> Templates.create_sidebar_items(extras)
        |> IO.iodata_to_binary()

      Jason.decode!(content)
    end
  end

  describe "module_page" do
    test "outputs the functions and docstrings", context do
      content = get_module_page([CompiledWithDocs], context)

      # Title and headers
      assert content =~ ~r{<title>CompiledWithDocs [^<]*</title>}

      assert content =~
               ~r{<span translate="no">CompiledWithDocs</span>\s*<small class=\"app-vsn\" translate="no">\(Elixir v1.0.1\)</small>}

      refute content =~ ~r{<small>module</small>}

      assert content =~
               ~r{moduledoc.*Example.*<span class="nc">CompiledWithDocs</span><span class="o">\.</span><span class="n">example</span>.*}ms

      assert content =~
               ~r{<h2 id="module-example-unicode-escaping" class="section-heading">.*<a href="#module-example-unicode-escaping" class="hover-link">.*<i class="ri-link-m" aria-hidden="true"></i>.*</a>.*<span class="text">Example ☃ Unicode &gt; escaping</span>.*</h2>}ms

      assert content =~
               ~r{<h3 id="module-example-h3-heading" class="section-heading">.*<a href="#module-example-h3-heading" class="hover-link">.*<i class="ri-link-m" aria-hidden="true"></i>.*</a>.*<span class="text">Example H3 heading</span>.*</h3>}ms

      # Summaries
      assert content =~ ~r{example/2.*Some example}ms
      assert content =~ ~r{example_without_docs/0.*<section class="docstring">.*</section>}ms
      assert content =~ ~r{example_1/0.*<span class="note">\(macro\)</span>}ms

      # Source
      assert content =~
               ~r{<a href="#{source_url()}/blob/master/test/fixtures/compiled_with_docs.ex#L1"[^>]*>\s*<i class="ri-code-s-slash-line" aria-hidden="true"></i>\s*<span class="sr-only">View Source</span>\s*</a>\s*}ms

      # Module annotations
      assert content =~ ~s{<span class=\"note\">(example_module_tag)</span>}

      # Functions
      assert content =~ ~s{<section class="detail" id="example/2">}
      assert content =~ ~s{<span id="example/1"></span>}
      assert content =~ ~s{example(foo, bar \\\\ Baz)}

      assert content =~
               ~r{<a href="#example/2" class="detail-link" title="Link to this function">\s*<i class="ri-link-m" aria-hidden="true"></i>\s*<span class="sr-only">Link to this function</span>\s*</a>}ms
    end

    test "outputs function groups", context do
      content =
        get_module_page([CompiledWithDocs], context,
          groups_for_docs: [
            "Example functions": &(&1[:purpose] == :example),
            Legacy: &is_binary(&1[:deprecated])
          ]
        )

      doc = EasyHTML.parse!(content)
      assert doc["#example-functions a[href='#example-functions']"]
      assert doc["#legacy a[href='#legacy']"]
      assert doc["#example-functions [id='example/2']"]
      refute doc["#legacy [id='example/2']"]
      assert doc["#functions [id='example_1/0']"]
      refute doc["#functions [id='example/2']"]
    end

    test "outputs deprecation information", context do
      content = get_module_page([CompiledWithDocs], context)

      assert content =~
               ~s{<span class="deprecated" title="Use something else instead">deprecated</span>}

      assert content =~
               ~r{<div class="deprecated">\s*This function is deprecated. Use something else instead.}
    end

    test "outputs the types and function specs", context do
      content = get_module_page([TypesAndSpecs, TypesAndSpecs.Sub], context)
      integer = ~s[<a href="https://hexdocs.pm/elixir/typespecs.html#basic-types">integer</a>()]

      public_html =
        ~S[<span class="attribute">@type</span> public(t) :: {t, ] <>
          ~s[<a href="https://hexdocs.pm/elixir/String.html#t:t/0">String.t</a>(), ] <>
          ~S[<a href="TypesAndSpecs.Sub.html#t:t/0">TypesAndSpecs.Sub.t</a>(), ] <>
          ~S[<a href="#t:opaque/0">opaque</a>(), :ok | :error}]

      assert content =~ ~s[<a href="#t:public/1" translate="no">public(t)</a>]
      refute content =~ ~s[<a href="#t:private/0">private</a>]
      assert content =~ public_html
      refute content =~ ~s[<strong>private\(t\)]
      assert content =~ ~s[A public type]
      assert content =~ ~s[<span class="attribute">@spec</span> add(]
      assert content =~ ~s[add(#{integer}, <a href="#t:opaque/0">opaque</a>()) :: #{integer}]
      refute content =~ ~s[minus(#{integer}, #{integer}) :: #{integer}]

      assert content =~
               ~s[Basic type: <a href=\"https://hexdocs.pm/elixir/typespecs.html#basic-types\"><code class=\"inline\">atom/0</code></a>.]

      assert content =~ ~r{<span class="attribute">@opaque</span> opaque}
    end

    test "outputs summaries", context do
      content = get_module_page([CompiledWithDocs], context)

      assert content =~
               ~r{<div class="summary-signature">\s*<a href="#example_1/0" translate="no">}
    end

    test "contains links to summary sections when those exist", context do
      content = get_module_page([CompiledWithDocs, CompiledWithDocs.Nested], context)
      refute content =~ ~r{types}
    end

    test "add hovers to <h3> tags", context do
      content = get_module_page([CompiledWithDocs], context)

      assert content =~
               ~r{<h3 id="example_with_h3/0-examples" class="section-heading">.*<a href="#example_with_h3/0-examples" class="hover-link">.*<i class="ri-link-m" aria-hidden="true"></i>.*</a>.*<span class="text">Examples</span>.*</h3>}ms
    end

    test "do not output overlapping functions, causing duplicate IDs", context do
      content = get_module_page([OverlappingDefaults], context)

      assert content =~ ~s{<section class="detail" id="overlapping_defaults/2">}
      assert content =~ ~s{<section class="detail" id="overlapping_defaults/3">}
      refute content =~ ~s{<span id="overlapping_defaults/2"></span>}

      assert content =~ ~s{<section class="detail" id="special_case/2">}

      assert content =~
               ~r{<section class="detail" id="special_case/4">\s*<span id="special_case/1"></span>\s*<span id="special_case/3"></span>}

      # This Regex checks for duplicate IDs
      refute content =~ ~r{id=("[^"]+").*id=\1}s
    end

    ## BEHAVIOURS

    test "outputs behavior and callbacks", context do
      content = get_module_page([CustomBehaviourOne], context)

      assert content =~
               ~r{<span translate="no">CustomBehaviourOne</span>\s*<small>behaviour</small>\s*<small class="app-vsn" translate="no">\(Elixir v1.0.1\)</small>\s*</h1>}

      assert content =~ ~r{Callbacks}
      assert content =~ ~r{<section class="detail" id="c:hello/1">}
      assert content =~ ~s[<span class="attribute">@callback</span> hello(]
      assert content =~ ~s[hello(%URI{})]
      assert content =~ ~s[greet(arg1)]

      content = get_module_page([CustomBehaviourTwo], context)

      assert content =~
               ~r{<span translate="no">CustomBehaviourTwo</span>\s*<small>behaviour</small>\s*<small class="app-vsn" translate="no">\(Elixir v1.0.1\)</small>\s*</h1>}

      assert content =~ ~r{Callbacks}
      assert content =~ ~r{<section class="detail" id="c:bye/1">}
      assert content =~ ~s[<span class="attribute">@macrocallback</span> bye(]
    end

    ## PROTOCOLS

    test "outputs the protocol type", context do
      content = get_module_page([CustomProtocol], context)

      assert content =~
               ~r{<span translate="no">CustomProtocol</span>\s*<small>protocol</small>\s*<small class=\"app-vsn\" translate="no">\(Elixir v1.0.1\)</small>\s*</h1>}
    end

    ## TASKS

    test "outputs the task type", context do
      content = get_module_page([Mix.Tasks.TaskWithDocs], context)

      assert content =~
               ~r{<span translate="no">mix task_with_docs</span>\s*<small class="app-vsn" translate="no">\(Elixir v1.0.1\)</small>\s*</h1>}
    end
  end
end
