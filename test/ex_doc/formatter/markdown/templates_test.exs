defmodule ExDoc.Formatter.MARKDOWN.TemplatesTest do
  use ExUnit.Case, async: true

  alias ExDoc.Formatter.HTML
  alias ExDoc.Formatter.MARKDOWN.Templates

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
      source_url_pattern: "#{source_url()}/blob/master/%{path}#L%{line}",
      homepage_url: homepage_url(),
      source_url: source_url(),
      output: "test/tmp/markdown_templates"
    }

    struct(default, config)
  end

  defp get_module_page(names, config \\ []) do
    config = doc_config(config)
    {mods, []} = ExDoc.Retriever.docs_from_modules(names, config)
    [mod | _] = HTML.render_all(mods, [], ".md", config, highlight_tag: "samp")
    Templates.module_page(config, mod)
  end

  setup_all do
    # File.mkdir_p!("test/tmp/markdown_templates")
    # File.cp_r!("formatters/markdown", "test/tmp/markdown_templates")
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
               ~S|<item id="XPTOModule" href="XPTOModule.md" media-type="application/md+xml" properties="scripted"/>|

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

      assert content =~ ~r{#\s*XPTOModule\s*}
    end

    test "outputs the functions and docstrings" do
      content = get_module_page([CompiledWithDocs])

      assert content =~ ~r{#\s*CompiledWithDocs\s*}

      assert content =~ ~s{# Summary</h1>}

      assert content =~
               ~r{## .*Example.*}ms

      assert content =~
               ~r{### .*Example H3 heading.*}ms

      assert content =~
               ~r{moduledoc.*Example.*<samp class="nc">CompiledWithDocs</samp><samp class="o">\.</samp><samp class="n">example</samp>.*}ms

      assert content =~ ~r{example/2.*Some example}ms
      assert content =~ ~r{example_without_docs/0.*}ms
      assert content =~ ~r{example_1/0.*> \(macro\)}ms

      assert content =~ ~s{example(foo, bar \\\\ Baz)}
    end

    test "outputs function groups" do
      content =
        get_module_page([CompiledWithDocs],
          groups_for_docs: [
            "Example functions": &(&1[:purpose] == :example),
            Legacy: &is_binary(&1[:deprecated])
          ]
        )

      assert content =~ ~r{.*Example functions}ms
      assert content =~ ~r{.*Legacy}ms
    end


    ## BEHAVIOURS

    test "outputs behavior and callbacks" do
      content = get_module_page([CustomBehaviourOne])

      assert content =~
               ~r{# \s*CustomBehaviourOne\s*behaviour\s*}m

      assert content =~ ~r{Callbacks}

      content = get_module_page([CustomBehaviourTwo])

      assert content =~
               ~r{# \s*CustomBehaviourTwo\s*behaviour\s*}m

      assert content =~ ~r{Callbacks}
    end

    ## PROTOCOLS

    test "outputs the protocol type" do
      content = get_module_page([CustomProtocol])
      assert content =~ ~r{# \s*CustomProtocol\s*protocol\s*}m
    end

    ## TASKS

    test "outputs the task type" do
      content = get_module_page([Mix.Tasks.TaskWithDocs])
      assert content =~ ~r{# \s*mix task_with_docs\s*}m
    end
  end
end
