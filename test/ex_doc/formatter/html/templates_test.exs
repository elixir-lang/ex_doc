defmodule ExDoc.Formatter.HTML.TemplatesTest do
  use ExUnit.Case, async: true

  alias ExDoc.Formatter
  alias ExDoc.Formatter.HTML.Templates

  @moduletag :tmp_dir

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
      source_url_pattern: fn path, line -> "#{source_url()}/blob/master/#{path}#L#{line}" end,
      homepage_url: homepage_url(),
      source_url: source_url(),
      output: context.tmp_dir <> "/html_templates"
    }

    struct(default, config)
  end

  defp get_module_page(names, context, config \\ []) do
    config = doc_config(context, config)
    {mods, []} = ExDoc.Retriever.docs_from_modules(names, config)
    [mod | _] = Formatter.render_all(mods, [], ".html", config, [])
    Templates.module_page(mod, config)
  end

  setup %{tmp_dir: tmp_dir} do
    File.cp_r!("formatters/html", tmp_dir <> "/html_templates")
    File.touch!(tmp_dir <> "/html_templates/dist/sidebar_items-123456.js")
    File.touch!(tmp_dir <> "/html_templates/dist/search_data-123456.js")
    :ok
  end

  describe "render_doc" do
    defp render_doc(doc) do
      doc
      |> ExDoc.DocAST.parse!("text/markdown")
      |> ExDoc.DocAST.add_ids_to_headers([:h2, :h3])
      |> Templates.render_doc()
    end

    test "adds fancy anchors around ids" do
      assert render_doc("""
             ## Foo

             ### Bar {:class=wrap}
             """) ==
               """
               <h2 id="foo" class="section-heading">
                 <a href="#foo" class="hover-link">
                   <i class="ri-link-m" aria-hidden="true"></i>
                 </a>
                 <span class="text">Foo</span>
               </h2>
               <h3 id="bar" class="wrap section-heading">
                 <a href="#bar" class="hover-link">
                   <i class="ri-link-m" aria-hidden="true"></i>
                 </a>
                 <span class="text">Bar</span>
               </h3>
               """
               |> String.replace(~r/\n\s*/, "")
    end

    test "skips fancy anchors on verbatim" do
      assert render_doc("""
             <h2>Foo</h2>
             """) == "<h2 id=\"foo\">Foo</h2>"
    end
  end

  describe "sidebar" do
    test "text links to homepage_url when set", context do
      content = Templates.sidebar_template(doc_config(context), :extra)

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

      content = Templates.sidebar_template(config, :extra)

      assert content =~
               ~r"""
               <div class="sidebar-header">\s*\
               <div class="sidebar-projectInfo">\s*\
               <div>\s*\
               <a href="hello.html" class="sidebar-projectName" translate="no">\s*Elixir\s*</a>\s*\
               <div class="sidebar-projectVersion" translate="no">\s*v1.0.1\s*</div>\s*\</div>\s*\</div>
               """
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

    test "includes api reference", context do
      names = [CompiledWithDocs]
      {nodes, []} = ExDoc.Retriever.docs_from_modules(names, doc_config(context))
      all = %{modules: nodes, tasks: []}

      assert [
               %{
                 "group" => "",
                 "headers" => [%{"anchor" => "modules", "id" => "Modules"}],
                 "id" => "api-reference",
                 "title" => "API Reference"
               }
             ] = create_sidebar_items(%{api_reference: true}, all, [])["extras"]

      assert [] = create_sidebar_items(%{api_reference: false}, all, [])["extras"]
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
             ] = create_sidebar_items(%{}, %{modules: nodes, tasks: []}, [])["modules"]
    end

    test "outputs deprecated: true if node is deprecated", context do
      names = [CompiledWithDocs]
      {nodes, []} = ExDoc.Retriever.docs_from_modules(names, doc_config(context))

      path = ["modules", Access.at!(0), "nodeGroups", Access.at!(0), "nodes"]

      sidebar_functions =
        get_in(create_sidebar_items(%{}, %{modules: nodes, tasks: []}, []), path)

      assert Enum.any?(
               sidebar_functions,
               &match?(%{"anchor" => "example/2", "deprecated" => true}, &1)
             )
    end

    test "outputs deprecated: true if module is deprecated", context do
      names = [Warnings]
      {nodes, []} = ExDoc.Retriever.docs_from_modules(names, doc_config(context))

      assert Enum.any?(
               create_sidebar_items(%{}, %{modules: nodes, tasks: []}, [])["modules"],
               &match?(%{"title" => "Warnings", "deprecated" => true}, &1)
             )
    end

    test "outputs nodes grouped based on metadata", context do
      {nodes, []} =
        ExDoc.Retriever.docs_from_modules(
          [CompiledWithDocs, CompiledWithDocs.Nested],
          doc_config(context,
            group_for_doc: fn metadata ->
              cond do
                metadata[:purpose] == :example -> "Example functions"
                is_binary(metadata[:deprecated]) -> "Legacy"
                true -> "Functions"
              end
            end,
            docs_groups: ["Example functions", "Legacy"]
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
             ] = create_sidebar_items(%{}, %{modules: nodes, tasks: []}, [])["modules"]
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
             ] = create_sidebar_items(%{}, %{modules: nodes, tasks: []}, [])["modules"]
    end

    test "builds sections out of moduledocs", context do
      names = [CompiledWithDocs, CompiledWithoutDocs, DuplicateHeadings]
      config = doc_config(context)
      {nodes, []} = ExDoc.Retriever.docs_from_modules(names, config)

      [compiled_with_docs, compiled_without_docs, duplicate_headings] =
        create_sidebar_items(%{}, %{modules: nodes, tasks: []}, [])["modules"]

      assert compiled_with_docs["sections"] == [
               %{
                 "anchor" => "module-example-unicode-escaping",
                 "id" => "Example ☃ Unicode > escaping"
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

    defp create_sidebar_items(config, nodes_map, extras) do
      "sidebarNodes=" <> content =
        config
        |> Templates.create_sidebar_items(nodes_map, extras)
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
               ~r{<a href="#example/2" class="detail-link" data-no-tooltip="" aria-label="Link to this function">\s*<i class="ri-link-m" aria-hidden="true"></i>\s*</a>}ms
    end

    test "outputs function groups", context do
      content =
        get_module_page([CompiledWithDocs], context,
          group_for_doc: fn metadata ->
            cond do
              metadata[:purpose] == :example -> "Example functions"
              is_binary(metadata[:deprecated]) -> "Legacy"
              true -> "Functions"
            end
          end
        )

      doc = LazyHTML.from_document(content)
      assert Enum.count(doc["#example-functions a[href='#example-functions']"]) == 1
      assert Enum.count(doc["#legacy a[href='#legacy']"]) == 1
      assert Enum.count(doc["#example-functions [id='example/2']"]) == 1
      assert Enum.count(doc["#legacy [id='example/2']"]) == 0
      assert Enum.count(doc["#functions [id='example_1/0']"]) == 1
      assert Enum.count(doc["#functions [id='example/2']"]) == 0
    end

    test "outputs groups descriptions", context do
      content =
        get_module_page([CompiledWithDocs], context,
          group_for_doc: fn metadata ->
            if metadata[:purpose] == :example do
              [
                title: "Example functions",
                description: """
                ### A section heading example

                A content example.

                See `example/1` or `example/2`.
                A link to `flatten/1`.
                """
              ]
            else
              "Functions"
            end
          end
        )

      doc = LazyHTML.from_document(content)
      assert Enum.count(doc["div.group-description"]) == 1
      assert Enum.count(doc["#group-description-example-functions"]) == 1
      assert Enum.count(doc["#group-description-example-functions h3"]) == 1
      assert Enum.count(doc["#group-example-functions-a-section-heading-example"]) == 1
      assert Enum.count(doc["#example-functions .group-description a[href='#example/1']"]) == 1
      assert Enum.count(doc["#example-functions .group-description a[href='#example/2']"]) == 1
      assert Enum.count(doc["#example-functions .group-description a[href='#flatten/1']"]) == 1

      assert content =~ ~s[<span class="text">A section heading example</span>]
      assert content =~ "<p>A content example.</p>"
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

      assert content =~ ~s[<a href="#t:public/1" data-no-tooltip="" translate="no">public(t)</a>]
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
               ~r{<div class="summary-signature">\s*<a href="#example_1/0" data-no-tooltip="" translate="no">}
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
