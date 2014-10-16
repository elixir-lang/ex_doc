defmodule ExDoc.Formatter.HTMLTest do
  use ExUnit.Case, async: true

  alias ExDoc.Formatter.HTML

  setup_all do
    File.mkdir(output_dir)
    File.copy("test/fixtures/README.md", "test/README.md")

    on_exit fn ->
      File.rm("test/README.md")
      System.cmd "rm", ["-rf", "#{output_dir}"]
    end

    :ok
  end

  defp output_dir do
    Path.expand("../../docs", __DIR__)
  end

  defp beam_dir do
    Path.expand("../../tmp/ebin", __DIR__)
  end

  defp doc_config do
    %ExDoc.Config{project: "Elixir",
                  version: "1.0.1",
                  output: "test/docs",
                  source_root: beam_dir,
                  readme: "test/README.md"}
  end

  defp get_modules(config \\ doc_config) do
    ExDoc.Retriever.docs_from_dir(beam_dir, config)
  end

  test "run generates the html file with the documentation" do
    HTML.run(get_modules, doc_config)

    assert File.regular?("#{output_dir}/CompiledWithDocs.html")
    assert File.regular?("#{output_dir}/CompiledWithDocs.Nested.html")
  end

  test "run generates in specified output directory" do
    config = %ExDoc.Config{output: "#{output_dir}/docs"}
    HTML.run(get_modules(config), config)

    assert File.regular?("#{output_dir}/docs/CompiledWithDocs.html")
    assert File.regular?("#{output_dir}/docs/index.html")
    assert File.regular?("#{output_dir}/docs/css/style.css")
  end

  test "run generates all listing files" do
    HTML.run(get_modules, doc_config)

    content = File.read!("#{output_dir}/modules_list.html")
    assert content =~ ~r{<li>.*"CompiledWithDocs\.html".*CompiledWithDocs.*<\/li>}ms
    assert content =~ ~r{<li>.*"CompiledWithDocs\.html#example\/2".*example\/2.*<\/li>}ms
    assert content =~ ~r{<li>.*"CompiledWithDocs.Nested\.html".*Nested.*<\/li>}ms
    assert content =~ ~r{<li>.*"UndefParent\.Nested\.html".*UndefParent\.Nested.*<\/li>}ms
    assert content =~ ~r{<li>.*"CustomBehaviour.html".*CustomBehaviour.*<\/li>}ms
    refute content =~ ~r{UndefParent\.Undocumented}ms

    content = File.read!("#{output_dir}/exceptions_list.html")
    assert content =~ ~r{<li>.*"RandomError\.html".*RandomError.*<\/li>}ms

    content = File.read!("#{output_dir}/protocols_list.html")
    assert content =~ ~r{<li>.*"CustomProtocol\.html".*CustomProtocol.*<\/li>}ms
  end

  test "run generates the overview file" do
    HTML.run(get_modules, doc_config)

    assert File.regular?("#{output_dir}/overview.html")
    content = File.read!("#{output_dir}/overview.html")
    assert content =~ ~r{<a href="CompiledWithDocs.html">CompiledWithDocs</a>}
    assert content =~ ~r{<p>moduledoc</p>}
    assert content =~ ~r{<a href="CompiledWithDocs.Nested.html">CompiledWithDocs.Nested</a>}
  end

  test "run generates the readme file" do
    HTML.run(get_modules, doc_config)

    assert File.regular?("#{output_dir}/README.html")
    content = File.read!("#{output_dir}/README.html")
    assert content =~ ~r{<a href="RandomError.html"><code>RandomError</code>}
    assert content =~ ~r{<a href="CustomBehaviourImpl.html#hello/1"><code>CustomBehaviourImpl.hello/1</code>}
    assert content =~ ~r{<a href="TypesAndSpecs.Sub.html"><code>TypesAndSpecs.Sub</code></a>}
  end

  test "make markdown codeblocks pretty" do
    with_empty_class = "<pre><code class=\"\">mix run --no-halt path/to/file.exs"
    without_class = "<pre><code>mix run --no-halt path/to/file.exs"

    assert HTML.pretty_codeblocks(with_empty_class) ==
           "<pre class=\"codeblock\">mix run --no-halt path/to/file.exs"
    assert HTML.pretty_codeblocks(without_class) ==
           "<pre class=\"codeblock\">mix run --no-halt path/to/file.exs"
  end
end
