defmodule ExDoc.Formatter.HTMLTest do
  use ExUnit.Case, async: false

  alias ExDoc.Formatter.HTML

  setup_all do
    {:ok, _} = File.copy("test/fixtures/README.md", "test/tmp/README.md")

    :ok
  end

  setup do
    {:ok, _} = File.rm_rf(output_dir)
    :ok = File.mkdir(output_dir)
    {:ok, []} = File.ls(output_dir)

    :ok
  end

  defp output_dir do
    Path.expand("../../tmp/doc", __DIR__)
  end

  defp beam_dir do
    Path.expand("../../tmp/ebin", __DIR__)
  end

  defp doc_config do
    [
      project: "Elixir",
      version: "1.0.1",
      formatter: "html",
      output: "test/tmp/doc",
      source_root: beam_dir,
      source_beam: beam_dir,
      readme: "test/tmp/README.md",
    ]
  end

  defp doc_config(config) do
    Keyword.merge(doc_config, config)
  end

  defp generate_docs(config) do
    ExDoc.generate_docs(config[:project], config[:version], config)
  end

  test "run generates the html file with the documentation and readme.html" do
    generate_docs(doc_config)

    assert File.regular?("#{output_dir}/CompiledWithDocs.html")
    assert File.regular?("#{output_dir}/CompiledWithDocs.Nested.html")
    assert File.regular?("#{output_dir}/readme.html")
    content = File.read!("#{output_dir}/index.html")
    assert content =~ ~r{<meta http-equiv="refresh" content="0; url=overview.html"\s*/>}
  end

  test "run generates in specified output directory and redirect index.html file" do
    config = doc_config([output: "#{output_dir}/another_dir", main: "RandomError", ])
    generate_docs(config)

    assert File.regular?("#{output_dir}/another_dir/CompiledWithDocs.html")
    assert File.regular?("#{output_dir}/another_dir/dist/app.css")
    assert File.regular?("#{output_dir}/another_dir/dist/app.js")
    assert File.regular?("#{output_dir}/another_dir/RandomError.html")
    content = File.read!("#{output_dir}/another_dir/index.html")
    assert content =~ ~r{<meta http-equiv="refresh" content="0; url=RandomError.html"\s*/>}
  end

  test "run generates all listing files" do
    generate_docs(doc_config)

    content = File.read!("#{output_dir}/dist/sidebar_items.js")
    assert content =~ ~r{.*"CompiledWithDocs\".*}ms
    assert content =~ ~r{.*"CompiledWithDocs\".*\"example/2\".*}ms
    assert content =~ ~r{.*"CompiledWithDocs.Nested\".*}ms
    assert content =~ ~r{.*"UndefParent\.Nested\".*}ms
    assert content =~ ~r{.*"CustomBehaviourOne\".*}ms
    assert content =~ ~r{.*"CustomBehaviourTwo\".*}ms
    refute content =~ ~r{UndefParent\.Undocumented}ms

    assert content =~ ~r{.*"RandomError\".*}ms

    assert content =~ ~r{.*"CustomProtocol\".*}ms
  end

  test "run generates the overview file" do
    generate_docs(doc_config)

    content = File.read!("#{output_dir}/overview.html")
    assert content =~ ~r{<a href="CompiledWithDocs.html">CompiledWithDocs</a>}
    assert content =~ ~r{<p>moduledoc</p>}
    assert content =~ ~r{<a href="CompiledWithDocs.Nested.html">CompiledWithDocs.Nested</a>}
  end

  test "run generates the readme file" do
    generate_docs(doc_config)

    content = File.read!("#{output_dir}/readme.html")
    assert content =~ ~r{<title>README [^<]*</title>}
    assert content =~ ~r{<a href="RandomError.html"><code>RandomError</code>}
    assert content =~ ~r{<a href="CustomBehaviourImpl.html#hello/1"><code>CustomBehaviourImpl.hello/1</code>}
    assert content =~ ~r{<a href="TypesAndSpecs.Sub.html"><code>TypesAndSpecs.Sub</code></a>}
  end

  test "run should not generate the readme file" do
    generate_docs(doc_config([readme: nil, ]))

    refute File.regular?("#{output_dir}/readme.html")
    content = File.read!("#{output_dir}/index.html")
    refute content =~ ~r{<title>README [^<]*</title>}
  end

  test "make markdown codeblocks pretty" do
    with_empty_class = "<pre><code class=\"\">mix run --no-halt path/to/file.exs"
    without_class = "<pre><code>mix run --no-halt path/to/file.exs"
    iex_detected_with_empty_class = "<pre><code class=\"\">iex&gt; max(4, 5)"
    iex_detected_without_class = "<pre><code>iex&gt; max(4, 5)"

    assert HTML.pretty_codeblocks(with_empty_class) ==
           "<pre><code class=\"elixir\">mix run --no-halt path/to/file.exs"
    assert HTML.pretty_codeblocks(without_class) ==
           "<pre><code class=\"elixir\">mix run --no-halt path/to/file.exs"
    assert HTML.pretty_codeblocks(iex_detected_with_empty_class) ==
          "<pre><code class=\"iex elixir\">iex&gt; max(4, 5)"
    assert HTML.pretty_codeblocks(iex_detected_without_class) ==
          "<pre><code class=\"iex elixir\">iex&gt; max(4, 5)"
  end

  test "run generates index.html and normalized options" do
    # 1. Check for output dir having trailing "/" stripped
    # 2. Check for default [main: "overview"]
    generate_docs doc_config([output: "#{output_dir}//", main: nil, ])

    content = File.read!("#{output_dir}/index.html")
    assert content =~ ~r{<meta http-equiv="refresh" content="0; url=overview.html"\s*/>}
    assert File.regular?("#{output_dir}/overview.html")
  end

  test "run generates trying to set 'main: index', should fail" do
    config = doc_config([main: "index"])
    assert_raise ArgumentError,
                 "\"main\" cannot be set to \"index\", otherwise it will recursively link to itself",
                 fn -> generate_docs(config) end
    assert {:ok, []} == File.ls "#{output_dir}"
  end

end
