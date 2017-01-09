defmodule ExDoc.Formatter.EPUBTest do
  use ExUnit.Case, async: true

  setup do
    File.rm_rf(output_dir())
    File.mkdir_p!(output_dir())
  end

  defp output_dir do
    Path.expand("../../tmp/epub", __DIR__)
  end

  defp beam_dir do
    Path.expand("../../tmp/beam", __DIR__)
  end

  defp doc_config do
    [project: "Elixir",
     version: "1.0.1",
     formatter: "epub",
     output: output_dir(),
     source_root: beam_dir(),
     source_beam: beam_dir(),
     extras: ["test/fixtures/README.md"]]
  end

  defp doc_config(config) do
    Keyword.merge(doc_config(), config)
  end

  defp generate_docs(config) do
    ExDoc.generate_docs(config[:project], config[:version], config)
  end

  defp generate_docs_and_unzip(options) do
    generate_docs(options)
    unzip_dir = "#{doc_config()[:output]}" |> String.to_char_list
    "#{doc_config()[:output]}/#{doc_config()[:project]}-v#{doc_config()[:version]}.epub"
    |> String.to_char_list
    |> :zip.unzip([cwd: unzip_dir])
  end

  test "check headers for module pages" do
    generate_docs_and_unzip doc_config([main: "RandomError"])

    content = File.read!("#{output_dir()}/OEBPS/RandomError.xhtml")
    assert content =~ ~r{<html.*xmlns:epub="http://www.idpf.org/2007/ops">}ms
    assert content =~ ~r{<meta charset="utf-8" />}ms
    assert content =~ ~r{<meta name="generator" content="ExDoc v[^"]+" />}
    assert content =~ ~r{<title>RandomError - Elixir v1.0.1</title>}
  end

  test "run generates assets with logo" do
    File.mkdir_p!("test/tmp/epub_assets/hello")
    File.touch!("test/tmp/epub_assets/hello/world.png")
    generate_docs_and_unzip(doc_config(assets: "test/tmp/epub_assets", logo: "test/fixtures/elixir.png"))
    assert File.regular?("#{output_dir()}/OEBPS/assets/hello/world.png")
    assert File.regular?("#{output_dir()}/OEBPS/assets/logo.png")
  after
    File.rm_rf!("test/tmp/epub_assets")
  end

  test "run stops when assets are invalid" do
    File.mkdir_p!("test/tmp/epub_assets/hello")
    File.touch!("test/tmp/epub_assets/hello/world.pdf")
    assert_raise(RuntimeError, ~s{asset with extension ".pdf" is not supported by EPUB format}, fn ->
      generate_docs(doc_config(assets: "test/tmp/epub_assets"))
    end)
  after
    File.rm_rf!("test/tmp/epub_assets")
  end

  test "run generates an EPUB file in the default directory" do
    generate_docs(doc_config())
    assert File.regular?("#{output_dir()}/#{doc_config()[:project]}-v#{doc_config()[:version]}.epub")
  end

  test "run generates an EPUB file in specified output directory" do
    config = doc_config([output: "#{output_dir()}/another_dir", main: "RandomError"])
    generate_docs(config)

    assert File.regular?("#{output_dir()}/another_dir/#{doc_config()[:project]}-v#{doc_config()[:version]}.epub")
  end

  test "run generates an EPUB file with a standardized structure" do
    generate_docs_and_unzip(doc_config())

    root_dir = "#{output_dir()}"
    meta_dir = "#{root_dir}/META-INF"
    oebps_dir = "#{root_dir}/OEBPS"
    dist_dir = "#{oebps_dir}/dist"

    assert File.regular?("#{root_dir}/mimetype")
    assert File.regular?("#{meta_dir}/container.xml")
    assert File.regular?("#{meta_dir}/com.apple.ibooks.display-options.xml")
    assert File.regular?("#{oebps_dir}/content.opf")
    assert File.regular?("#{oebps_dir}/nav.xhtml")
    assert File.regular?("#{oebps_dir}/title.xhtml")
    assert File.regular?("#{oebps_dir}/readme.xhtml")
    assert File.regular?("#{oebps_dir}/CompiledWithDocs.xhtml")
    assert File.regular?("#{oebps_dir}/CompiledWithDocs.Nested.xhtml")
    assert "#{dist_dir}/epub*.css" |> Path.wildcard() |> List.first() |> File.regular?()
    assert "#{dist_dir}/app*.js" |> Path.wildcard() |> List.first() |> File.regular?()
  end

  test "run generates all listing files" do
    generate_docs_and_unzip(doc_config())

    content = File.read!("#{output_dir()}/OEBPS/content.opf")

    assert content =~ ~r{.*"CompiledWithDocs\".*}ms
    assert content =~ ~r{.*"CompiledWithDocs.Nested\".*}ms
    assert content =~ ~r{.*"UndefParent\.Nested\".*}ms
    assert content =~ ~r{.*"CustomBehaviourOne\".*}ms
    assert content =~ ~r{.*"CustomBehaviourTwo\".*}ms
    refute content =~ ~r{UndefParent\.Undocumented}ms
    assert content =~ ~r{.*"RandomError\".*}ms
    assert content =~ ~r{.*"CustomProtocol\".*}ms
  end

  test "run generates the readme file" do
    config = doc_config([main: "README"])
    generate_docs_and_unzip(config)

    content = File.read!("#{output_dir()}/OEBPS/readme.xhtml")
    assert content =~ ~r{<title>README [^<]*</title>}
    assert content =~ ~r{<a href="RandomError.xhtml"><code>RandomError</code>}
    assert content =~ ~r{<a href="CustomBehaviourImpl.xhtml#hello/1"><code>CustomBehaviourImpl.hello/1</code>}
    assert content =~ ~r{<a href="TypesAndSpecs.Sub.xhtml"><code>TypesAndSpecs.Sub</code></a>}

    content = File.read!("#{output_dir()}/OEBPS/nav.xhtml")
    assert content =~ ~r{<li><a href="readme.xhtml">README</a></li>}
  end

  test "run should not generate the readme file" do
    generate_docs_and_unzip(doc_config([extras: []]))
    refute File.regular?("#{output_dir()}/OEBPS/readme.xhtml")

    content = File.read!("#{output_dir()}/OEBPS/content.opf")
    refute content =~ ~r{<title>README [^<]*</title>}

    content = File.read!("#{output_dir()}/OEBPS/nav.xhtml")
    refute content =~ ~r{<li><a href="README.html">README</a></li>}
  end
end
