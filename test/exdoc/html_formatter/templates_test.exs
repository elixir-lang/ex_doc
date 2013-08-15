Code.require_file "../../../test_helper.exs", __FILE__

defmodule ExDoc.HTMLFormatterTest.TemplatesTest do
  use ExUnit.Case
  alias ExDoc.HTMLFormatter.Templates

  defp input_path do
    Path.expand("test/tmp/Elixir")
  end

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
    files   = [
      "#{input_path}.CompiledWithDocs.beam",
      "#{input_path}.CompiledWithDocs.Nested.beam"
    ]
    nodes   = ExDoc.Retriever.docs_from_files(files, doc_config)
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
end
