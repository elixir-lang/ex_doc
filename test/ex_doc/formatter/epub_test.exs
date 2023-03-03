defmodule ExDoc.Formatter.EPUBTest do
  use ExUnit.Case, async: true

  @moduletag :tmp_dir

  @before_closing_head_tag_content_epub "UNIQUE:<dont-escape>&copy;BEFORE-CLOSING-HEAD-TAG-HTML</dont-escape>"
  @before_closing_body_tag_content_epub "UNIQUE:<dont-escape>&copy;BEFORE-CLOSING-BODY-TAG-HTML</dont-escape>"

  defp before_closing_head_tag(:epub), do: @before_closing_head_tag_content_epub
  defp before_closing_body_tag(:epub), do: @before_closing_body_tag_content_epub

  def before_closing_head_tag(:epub, name), do: "<meta name=#{name}>"
  def before_closing_body_tag(:epub, name), do: "<p>#{name}</p>"

  defp doc_config(%{tmp_dir: tmp_dir} = _context) do
    [
      app: :elixir,
      project: "Elixir",
      version: "1.0.1",
      formatter: "epub",
      output: tmp_dir <> "/epub",
      source_beam: "test/tmp/beam",
      extras: ["test/fixtures/README.md"],
      skip_undefined_reference_warnings_on: ["Warnings"]
    ]
  end

  defp doc_config(context, config) when is_map(context) and is_list(config) do
    Keyword.merge(doc_config(context), config)
  end

  defp generate_docs(config) do
    ExDoc.generate_docs(config[:project], config[:version], config)
  end

  defp generate_docs_and_unzip(context, config) do
    generate_docs(config)
    unzip_dir = String.to_charlist("#{doc_config(context)[:output]}")

    "#{doc_config(context)[:output]}/#{doc_config(context)[:project]}.epub"
    |> String.to_charlist()
    |> :zip.unzip(cwd: unzip_dir)
  end

  test "generates headers for module pages", %{tmp_dir: tmp_dir} = context do
    generate_docs_and_unzip(context, doc_config(context, main: "RandomError"))

    content = File.read!(tmp_dir <> "/epub/OEBPS/RandomError.xhtml")
    assert content =~ ~r{<html.*lang="en".*xmlns:epub="http://www.idpf.org/2007/ops">}ms
    assert content =~ ~r{<meta charset="utf-8" />}ms
    assert content =~ ~r{<meta name="generator" content="ExDoc v[^"]+" />}
    assert content =~ ~r{<title>RandomError - Elixir v1.0.1</title>}
  end

  test "allows to set the primary language of the document", %{tmp_dir: tmp_dir} = context do
    generate_docs_and_unzip(context, doc_config(context, main: "RandomError", language: "fr"))

    content = File.read!(tmp_dir <> "/epub/OEBPS/RandomError.xhtml")
    assert content =~ ~r{<html.*lang="fr".*xmlns:epub="http://www.idpf.org/2007/ops">}ms
  end

  test "allows to set the authors of the book", %{tmp_dir: tmp_dir} = context do
    generate_docs_and_unzip(context, doc_config(context, authors: ["John Doe", "Jane Doe"]))

    content = File.read!(tmp_dir <> "/epub/OEBPS/content.opf")
    assert content =~ ~r{<dc:creator id="author1">John Doe</dc:creator>}
    assert content =~ ~r{<dc:creator id="author2">Jane Doe</dc:creator>}
  end

  test "raises when assets are invalid", context do
    File.mkdir_p!("test/tmp/epub_assets/hello")
    File.touch!("test/tmp/epub_assets/hello/world.pdf")

    assert_raise(
      RuntimeError,
      ~s{asset with extension ".pdf" is not supported by EPUB format},
      fn -> generate_docs(doc_config(context, assets: "test/tmp/epub_assets")) end
    )
  after
    File.rm_rf!("test/tmp/epub_assets")
  end

  test "generates an EPUB file in the default directory", %{tmp_dir: tmp_dir} = context do
    generate_docs(doc_config(context))
    assert File.regular?(tmp_dir <> "/epub/#{doc_config(context)[:project]}.epub")
  end

  test "generates an EPUB file with erlang as proglang", %{tmp_dir: tmp_dir} = context do
    config =
      context
      |> doc_config()
      |> Keyword.put(:proglang, :erlang)
      |> Keyword.update!(:skip_undefined_reference_warnings_on, &["test/fixtures/README.md" | &1])

    generate_docs(config)
    assert File.regular?(tmp_dir <> "/epub/#{config[:project]}.epub")
  end

  test "generates an EPUB file in specified output directory", %{tmp_dir: tmp_dir} = context do
    config = doc_config(context, output: tmp_dir <> "/epub/another_dir", main: "RandomError")
    generate_docs(config)

    assert File.regular?(tmp_dir <> "/epub/another_dir/#{doc_config(context)[:project]}.epub")
  end

  test "generates an EPUB file with a standardized structure", %{tmp_dir: tmp_dir} = context do
    generate_docs_and_unzip(context, doc_config(context))

    root_dir = tmp_dir <> "/epub"
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
    assert [_] = Path.wildcard("#{dist_dir}/epub-*.js")
    assert [_] = Path.wildcard("#{dist_dir}/epub-elixir-*.css")
  end

  test "generates all listing files", %{tmp_dir: tmp_dir} = context do
    generate_docs_and_unzip(context, doc_config(context))
    content = File.read!(tmp_dir <> "/epub/OEBPS/content.opf")

    assert content =~ ~r{.*"CompiledWithDocs\".*}ms
    assert content =~ ~r{.*"CompiledWithDocs.Nested\".*}ms
    assert content =~ ~r{.*"CustomBehaviourOne\".*}ms
    assert content =~ ~r{.*"CustomBehaviourTwo\".*}ms
    assert content =~ ~r{.*"RandomError\".*}ms
    assert content =~ ~r{.*"CustomProtocol\".*}ms
    assert content =~ ~r{.*"Mix\.Tasks\.TaskWithDocs\".*}ms
  end

  test "generates the readme file", %{tmp_dir: tmp_dir} = context do
    config = doc_config(context, main: "README")
    generate_docs_and_unzip(context, config)

    content = File.read!(tmp_dir <> "/epub/OEBPS/readme.xhtml")
    assert content =~ ~r{<title>README [^<]*</title>}
    assert content =~ ~r{<a href="RandomError.xhtml"><code(\sclass="inline")?>RandomError</code>}

    assert content =~
             ~r{<a href="CustomBehaviourImpl.xhtml#hello/1"><code(\sclass="inline")?>CustomBehaviourImpl.hello/1</code>}

    assert content =~
             ~r{<a href="TypesAndSpecs.Sub.xhtml"><code(\sclass="inline")?>TypesAndSpecs.Sub</code></a>}

    content = File.read!(tmp_dir <> "/epub/OEBPS/nav.xhtml")
    assert content =~ ~r{<li><a href="readme.xhtml">README</a></li>}
  end

  test "uses samp as highlight tag for markdown", %{tmp_dir: tmp_dir} = context do
    generate_docs_and_unzip(context, doc_config(context))

    assert File.read!(tmp_dir <> "/epub/OEBPS/CompiledWithDocs.xhtml") =~
             "<samp class=\"nc\">CompiledWithDocs<\/samp>"
  end

  @example_basenames [
    # "structural" pages
    "nav.xhtml",
    "title.xhtml",
    "readme.xhtml",
    # "module pages"
    "CompiledWithDocs.xhtml",
    "CompiledWithDocs.Nested.xhtml"
  ]

  test "before_closing_*_tags required by the user are in the right place",
       %{tmp_dir: tmp_dir} = context do
    generate_docs_and_unzip(
      context,
      doc_config(context,
        before_closing_head_tag: &before_closing_head_tag/1,
        before_closing_body_tag: &before_closing_body_tag/1
      )
    )

    oebps_dir = tmp_dir <> "/epub/OEBPS"

    for basename <- @example_basenames do
      content = File.read!(Path.join(oebps_dir, basename))
      assert content =~ ~r[#{@before_closing_head_tag_content_epub}\s*</head>]
      assert content =~ ~r[#{@before_closing_body_tag_content_epub}\s*</body>]
    end
  end

  test "before_closing_*_tags required by the user are in the right place using MFA",
       %{tmp_dir: tmp_dir} = context do
    generate_docs_and_unzip(
      context,
      doc_config(context,
        before_closing_head_tag: {__MODULE__, :before_closing_head_tag, ["Demo"]},
        before_closing_body_tag: {__MODULE__, :before_closing_body_tag, ["Demo"]}
      )
    )

    oebps_dir = tmp_dir <> "/epub/OEBPS"

    for basename <- @example_basenames do
      content = File.read!(Path.join(oebps_dir, basename))
      assert content =~ ~r[<meta name=Demo>\s*</head>]
      assert content =~ ~r[<p>Demo</p>\s*</body>]
    end
  end

  test "assets required by the user end up in the right place", %{tmp_dir: tmp_dir} = context do
    File.mkdir_p!("test/tmp/epub_assets/hello")
    File.touch!("test/tmp/epub_assets/hello/world.png")

    generate_docs_and_unzip(
      context,
      doc_config(context,
        assets: "test/tmp/epub_assets",
        logo: "test/fixtures/elixir.png",
        cover: "test/fixtures/elixir.png"
      )
    )

    assert File.regular?(tmp_dir <> "/epub/OEBPS/assets/hello/world.png")
    assert File.regular?(tmp_dir <> "/epub/OEBPS/assets/logo.png")
    assert File.regular?(tmp_dir <> "/epub/OEBPS/assets/cover.png")
  after
    File.rm_rf!("test/tmp/epub_assets")
  end
end
