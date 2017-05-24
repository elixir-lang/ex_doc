defmodule ExDoc.Formatter.EPUB.TemplatesTest do
  use ExUnit.Case, async: true

  alias ExDoc.Formatter.HTML
  alias ExDoc.Formatter.EPUB.Templates

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
      output: "test/tmp/epub_templates"
    }
  end

  defp get_module_page(names) do
    mods =
      names
      |> ExDoc.Retriever.docs_from_modules(doc_config())
      |> HTML.Autolink.all(".xhtml", [])
    Templates.module_page(doc_config(), hd(mods))
  end

  setup_all do
    File.mkdir_p!("test/tmp/epub_templates/OEBPS")
    File.cp_r!("priv/ex_doc/formatter/epub/assets", "test/tmp/epub_templates/OEBPS")
    :ok
  end

  ## MODULES

  test "module_page generates only the module name when there's no more info" do
    module_node = %ExDoc.ModuleNode{module: XPTOModule, doc: nil, id: "XPTOModule", title: "XPTOModule"}
    content = Templates.module_page(doc_config(), module_node)

    assert content =~ ~r{<title>XPTOModule [^<]*</title>}
    assert content =~ ~r{<h1 id="content">\s*XPTOModule\s*}
  end

  test "module_page outputs the functions and docstrings" do
    content = get_module_page([CompiledWithDocs])

    assert content =~ ~r{<title>CompiledWithDocs [^<]*</title>}
    assert content =~ ~r{<h1 id="content">\s*CompiledWithDocs\s*}
    assert content =~ ~r{moduledoc.*Example.*CompiledWithDocs\.example.*}ms
    assert content =~ ~r{example/2.*Some example}ms
    assert content =~ ~r{example_without_docs/0.*<section class="docstring">.*</section>}ms
    assert content =~ ~r{example_1/0.*<span class="note">\(macro\)</span>}ms

    assert content =~ ~s{<div class="detail" id="example_1/0">}
    assert content =~ ~s{example(foo, bar \\\\ Baz)}
  end

  test "module_page outputs summaries" do
    content = get_module_page([CompiledWithDocs])
    assert content =~ ~r{<div class="summary-signature">\s*<a href="#example_1/0">}
  end

  test "module_page contains links to summary sections when those exist" do
    content = get_module_page([CompiledWithDocs, CompiledWithDocs.Nested])
    refute content =~ ~r{types_details}
  end

  ## BEHAVIOURS

  test "module_page outputs behavior and callbacks" do
    content = get_module_page([CustomBehaviourOne])
    assert content =~ ~r{<h1 id="content">\s*CustomBehaviourOne\s*<small>behaviour</small>\s*</h1>}m
    assert content =~ ~r{Callbacks}
    assert content =~ ~r{<div class="detail" id="c:hello/1">}

    content = get_module_page([CustomBehaviourTwo])
    assert content =~ ~r{<h1 id="content">\s*CustomBehaviourTwo\s*<small>behaviour</small>\s*</h1>}m
    assert content =~ ~r{Callbacks}
    assert content =~ ~r{<div class="detail" id="c:bye/1">}
  end

  ## PROTOCOLS

  test "module_page outputs the protocol type" do
    content = get_module_page([CustomProtocol])
    assert content =~ ~r{<h1 id="content">\s*CustomProtocol\s*<small>protocol</small>\s*}m
  end

  ## TASKS

  test "module_page outputs the task type" do
    content = get_module_page([Mix.Tasks.TaskWithDocs])
    assert content =~ ~r{<h1 id="content">\s*mix task_with_docs\s*}m
  end
end
