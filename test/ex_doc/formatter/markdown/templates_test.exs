defmodule ExDoc.Formatter.MARKDOWN.TemplatesTest do
  use ExUnit.Case, async: true

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
    [mod | _] = ExDoc.Formatter.render_all(mods, [], ".md", config, highlight_tag: "samp")
    Templates.module_page(config, mod)
  end

  setup_all do
    # File.mkdir_p!("test/tmp/markdown_templates")
    # File.cp_r!("formatters/markdown", "test/tmp/markdown_templates")
    :ok
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
               ~r{moduledoc.*Example.*CompiledWithDocs\.example.*}ms

      assert content =~ ~r{Some example}ms
      assert content =~ ~r{example_without_docs().*}ms
      assert content =~ ~r{example_1().*> \(macro\)}ms

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
               ~r{# CustomBehaviourOne \(behaviour\)}m

      assert content =~ ~r{Callbacks}

      content = get_module_page([CustomBehaviourTwo])

      assert content =~
               ~r{# CustomBehaviourTwo \(behaviour\)}m

      assert content =~ ~r{Callbacks}
    end

    ## PROTOCOLS

    test "outputs the protocol type" do
      content = get_module_page([CustomProtocol])
      assert content =~ ~r{# CustomProtocol \(protocol\)}m
    end

    ## TASKS

    test "outputs the task type" do
      content = get_module_page([Mix.Tasks.TaskWithDocs])
      assert content =~ ~r{#\s*mix task_with_docs}m
    end
  end
end
