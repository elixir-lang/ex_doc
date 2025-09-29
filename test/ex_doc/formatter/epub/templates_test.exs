defmodule ExDoc.Formatter.EPUB.TemplatesTest do
  use ExUnit.Case, async: true

  alias ExDoc.{Formatter}
  alias ExDoc.Formatter.EPUB.Templates

  defp source_url do
    "https://github.com/elixir-lang/elixir"
  end

  defp homepage_url do
    "https://elixir-lang.org"
  end

  defp doc_config(config \\ []) do
    default = %ExDoc.Config{
      project: "Elixir",
      version: "1.0.1",
      source_url_pattern: fn path, line -> "#{source_url()}/blob/master/#{path}#L#{line}" end,
      homepage_url: homepage_url(),
      source_url: source_url(),
      output: "test/tmp/epub_templates"
    }

    struct(default, config)
  end

  defp get_module_page(names, config \\ []) do
    config = doc_config(config)
    {mods, []} = ExDoc.Retriever.docs_from_modules(names, config)
    [mod | _] = Formatter.render_all(mods, [], ".xhtml", config, highlight_tag: "samp")
    Templates.module_page(config, mod)
  end

  setup_all do
    File.mkdir_p!("test/tmp/epub_templates/OEBPS")
    File.cp_r!("formatters/epub", "test/tmp/epub_templates/OEBPS")
    :ok
  end

  describe "content_template/5" do
    test "includes logo as a resource if specified in the config" do
      nodes = %{modules: [], tasks: []}

      content =
        [logo: "my_logo.png"]
        |> doc_config()
        |> Templates.content_template(nodes, "uuid", "datetime", _static_files = [])

      assert content =~ ~S|<item id="logo" href="assets/logo.png" media-type="image/png"/>|
    end

    test "includes cover as a resource if specified in the config" do
      nodes = %{modules: [], tasks: []}

      content =
        [cover: "my_cover.svg"]
        |> doc_config()
        |> Templates.content_template(nodes, "uuid", "datetime", _static_files = [])

      assert content =~ ~S|<meta name="cover" content="cover-image"/>|

      assert content =~
               ~S|<item id="cover-image" href="assets/cover.svg" media-type="image/svg+xml"/>|
    end

    test "includes modules as a resource" do
      module_node = %ExDoc.ModuleNode{
        module: XPTOModule,
        doc: nil,
        id: "XPTOModule",
        title: "XPTOModule"
      }

      nodes = %{modules: [module_node], tasks: []}

      content =
        Templates.content_template(doc_config(), nodes, "uuid", "datetime", _static_files = [])

      assert content =~
               ~S|<item id="XPTOModule" href="XPTOModule.xhtml" media-type="application/xhtml+xml" properties="scripted"/>|

      assert content =~ ~S|<itemref idref="XPTOModule"/>|
    end
  end

  describe "module_page/2" do
    test "generates only the module name when there's no more info" do
      module_node = %ExDoc.ModuleNode{
        module: XPTOModule,
        doc: nil,
        id: "XPTOModule",
        title: "XPTOModule"
      }

      content = Templates.module_page(doc_config(), module_node)

      assert content =~ ~r{<title>XPTOModule [^<]*</title>}
      assert content =~ ~r{<h1 id="content">\s*XPTOModule\s*}
    end

    test "outputs the functions and docstrings" do
      content = get_module_page([CompiledWithDocs])

      assert content =~ ~r{<title>CompiledWithDocs [^<]*</title>}
      assert content =~ ~r{<h1 id="content">\s*CompiledWithDocs\s*}

      assert content =~ ~s{<h1 class="section-heading">Summary</h1>}

      assert content =~
               ~r{<h2 id="module-example-unicode-escaping">.*Example.*</h2>}ms

      assert content =~
               ~r{<h3 id="module-example-h3-heading">.*Example H3 heading.*</h3>}ms

      assert content =~
               ~r{moduledoc.*Example.*<samp class="nc">CompiledWithDocs</samp><samp class="o">\.</samp><samp class="n">example</samp>.*}ms

      assert content =~ ~r{example/2.*Some example}ms
      assert content =~ ~r{example_without_docs/0.*<section class="docstring">.*</section>}ms
      assert content =~ ~r{example_1/0.*<span class="note">\(macro\)</span>}ms

      assert content =~ ~s{<section class="detail" id="example_1/0">}
      assert content =~ ~s{example(foo, bar \\\\ Baz)}
    end

    test "outputs function groups" do
      content =
        get_module_page([CompiledWithDocs],
          group_for_doc: fn metadata ->
            cond do
              metadata[:purpose] == :example -> "Example functions"
              is_binary(metadata[:deprecated]) -> "Legacy"
              true -> "Functions"
            end
          end,
          docs_groups: ["Example functions", "Legacy"]
        )

      assert content =~ ~r{id="example-functions".*Example functions}ms
      assert content =~ ~r{id="legacy".*Legacy}ms
      assert content =~ ~r{id="example-functions".*id="example/2"}ms
      refute content =~ ~r{id="legacy".*id="example/2"}ms
      refute content =~ ~r{id="functions".*id="example/2"}ms
      assert content =~ ~r{id="functions".*id="example_1/0"}ms
    end

    test "outputs groups descriptions" do
      content =
        get_module_page([CompiledWithDocs],
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

      assert content =~
               ~s[<h3 id="group-example-functions-a-section-heading-example">A section heading example</h3>]

      assert content =~ "<p>A content example.</p>"
    end

    test "outputs summaries" do
      content = get_module_page([CompiledWithDocs])

      assert content =~
               ~r{<div class="summary-signature">\s*<a href="#example_1/0" data-no-tooltip="" translate="no">}
    end

    test "contains links to summary sections when those exist" do
      content = get_module_page([CompiledWithDocs, CompiledWithDocs.Nested])
      refute content =~ ~r{types_details}
    end

    ## BEHAVIOURS

    test "outputs behavior and callbacks" do
      content = get_module_page([CustomBehaviourOne])

      assert content =~
               ~r{<h1 id="content">\s*CustomBehaviourOne\s*<small>behaviour</small>\s*</h1>}m

      assert content =~ ~r{Callbacks}
      assert content =~ ~r{<section class="detail" id="c:hello/1">}

      content = get_module_page([CustomBehaviourTwo])

      assert content =~
               ~r{<h1 id="content">\s*CustomBehaviourTwo\s*<small>behaviour</small>\s*</h1>}m

      assert content =~ ~r{Callbacks}
      assert content =~ ~r{<section class="detail" id="c:bye/1">}
    end

    ## PROTOCOLS

    test "outputs the protocol type" do
      content = get_module_page([CustomProtocol])
      assert content =~ ~r{<h1 id="content">\s*CustomProtocol\s*<small>protocol</small>\s*}m
    end

    ## TASKS

    test "outputs the task type" do
      content = get_module_page([Mix.Tasks.TaskWithDocs])
      assert content =~ ~r{<h1 id="content">\s*mix task_with_docs\s*}m
    end
  end
end
