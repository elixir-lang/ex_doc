defmodule ExDoc.Formatter.EPUBTest do
  use ExUnit.Case
  alias ExDoc.Markdown.DummyProcessor

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

  # The following module attributes contain the values for user-required content.
  # Content required by the custom markdown processor is defined in its own module,
  # and will be accessed as `DummyProcessor.before_closing_*_tag(:html)`
  @before_closing_head_tag_content_epub "UNIQUE:<dont-escape>&copy;BEFORE-CLOSING-HEAD-TAG-HTML</dont-escape>"
  @before_closing_body_tag_content_epub "UNIQUE:<dont-escape>&copy;BEFORE-CLOSING-BODY-TAG-HTML</dont-escape>"
  @before_closing_head_tag_content_epub "UNIQUE:<dont-escape>&copy;BEFORE-CLOSING-HEAD-TAG-EPUB</dont-escape>"
  @before_closing_body_tag_content_epub "UNIQUE:<dont-escape>&copy;BEFORE-CLOSING-BODY-TAG-EPUB</dont-escape>"

  defp before_closing_head_tag(:html), do: @before_closing_head_tag_content_epub
  defp before_closing_head_tag(:epub), do: @before_closing_head_tag_content_epub

  defp before_closing_body_tag(:html), do: @before_closing_body_tag_content_epub
  defp before_closing_body_tag(:epub), do: @before_closing_body_tag_content_epub

  defp doc_config do
    [
      project: "Elixir",
      version: "1.0.1",
      formatter: "epub",
      output: output_dir(),
      source_root: beam_dir(),
      source_beam: beam_dir(),
      extras: ["test/fixtures/README.md"],
      skip_undefined_reference_warnings_on: ["Warnings"]
    ]
  end

  defp doc_config(config) do
    Keyword.merge(doc_config(), config)
  end

  defp generate_docs(config) do
    ExDoc.generate_docs(config[:project], config[:version], config)
  end

  defp generate_docs_and_unzip(options) do
    generate_docs(options)
    unzip_dir = String.to_charlist("#{doc_config()[:output]}")

    "#{doc_config()[:output]}/#{doc_config()[:project]}.epub"
    |> String.to_charlist()
    |> :zip.unzip(cwd: unzip_dir)
  end

  test "generates headers for module pages" do
    generate_docs_and_unzip(doc_config(main: "RandomError"))

    content = File.read!("#{output_dir()}/OEBPS/RandomError.xhtml")
    assert content =~ ~r{<html.*lang="en".*xmlns:epub="http://www.idpf.org/2007/ops">}ms
    assert content =~ ~r{<meta charset="utf-8" />}ms
    assert content =~ ~r{<meta name="generator" content="ExDoc v[^"]+" />}
    assert content =~ ~r{<title>RandomError - Elixir v1.0.1</title>}
  end

  test "allows to set the primary language of the document" do
    generate_docs_and_unzip(doc_config(main: "RandomError", language: "fr"))

    content = File.read!("#{output_dir()}/OEBPS/RandomError.xhtml")
    assert content =~ ~r{<html.*lang="fr".*xmlns:epub="http://www.idpf.org/2007/ops">}ms
  end

  test "raises when assets are invalid" do
    File.mkdir_p!("test/tmp/epub_assets/hello")
    File.touch!("test/tmp/epub_assets/hello/world.pdf")

    assert_raise(
      RuntimeError,
      ~s{asset with extension ".pdf" is not supported by EPUB format},
      fn -> generate_docs(doc_config(assets: "test/tmp/epub_assets")) end
    )
  after
    File.rm_rf!("test/tmp/epub_assets")
  end

  test "generates an EPUB file in the default directory" do
    generate_docs(doc_config())
    assert File.regular?("#{output_dir()}/#{doc_config()[:project]}.epub")
  end

  test "generates an EPUB file in specified output directory" do
    config = doc_config(output: "#{output_dir()}/another_dir", main: "RandomError")
    generate_docs(config)

    assert File.regular?("#{output_dir()}/another_dir/#{doc_config()[:project]}.epub")
  end

  test "generates an EPUB file with a standardized structure" do
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
    assert "#{dist_dir}/epub*.js" |> Path.wildcard() |> List.first() |> File.regular?()
  end

  test "generates all listing files" do
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
    assert content =~ ~r{.*"Mix\.Tasks\.TaskWithDocs\".*}ms
  end

  test "generates the readme file" do
    config = doc_config(main: "README")
    generate_docs_and_unzip(config)

    content = File.read!("#{output_dir()}/OEBPS/readme.xhtml")
    assert content =~ ~r{<title>README [^<]*</title>}

    assert content =~
             ~r{<a href="RandomError.xhtml" title="RandomError module"><code(\sclass="inline")?>RandomError</code>}

    assert content =~
             ~r{<a href="CustomBehaviourImpl.xhtml#hello/1" title="CustomBehaviourImpl.hello/1"><code(\sclass="inline")?>CustomBehaviourImpl.hello/1</code>}

    assert content =~
             ~r{<a href="TypesAndSpecs.Sub.xhtml" title="TypesAndSpecs.Sub module"><code(\sclass="inline")?>TypesAndSpecs.Sub</code></a>}

    content = File.read!("#{output_dir()}/OEBPS/nav.xhtml")
    assert content =~ ~r{<li><a href="readme.xhtml">README</a></li>}
  end

  describe "before_closing_*_tags" do
    # There are 3 possibilities for the `before_closing_*_tags`:
    # - required by the user alone
    # - required by the markdown processor alone
    # - required by both the markdown processor and the user
    # We will test the three possibilities independently

    @example_basenames [
      # "structural" pages
      "nav.xhtml",
      "title.xhtml",
      "readme.xhtml",
      # "module pages"
      "CompiledWithDocs.xhtml",
      "CompiledWithDocs.Nested.xhtml"
    ]

    # 1. required by the user
    test "required by the user are in the right place" do
      generate_docs_and_unzip(
        doc_config(
          before_closing_head_tag: &before_closing_head_tag/1,
          before_closing_body_tag: &before_closing_body_tag/1
        )
      )

      oebps_dir = "#{output_dir()}/OEBPS"

      for basename <- @example_basenames do
        content = File.read!(Path.join(oebps_dir, basename))
        assert content =~ ~r[#{@before_closing_head_tag_content_epub}\s*</head>]
        assert content =~ ~r[#{@before_closing_body_tag_content_epub}\s*</body>]

        assert not (content =~ ~r[#{DummyProcessor.before_closing_head_tag(:epub)}\s*</head>])
        assert not (content =~ ~r[#{DummyProcessor.before_closing_body_tag(:epub)}\s*</body>])
      end
    end

    # 2. Required by the markdown processor
    test "required by the markdown processor are in the right place" do
      generate_docs_and_unzip(doc_config(markdown_processor: DummyProcessor))
      oebps_dir = "#{output_dir()}/OEBPS"

      for basename <- @example_basenames do
        content = File.read!(Path.join(oebps_dir, basename))
        assert content =~ ~r[#{DummyProcessor.before_closing_head_tag(:epub)}\s*</head>]
        assert content =~ ~r[#{DummyProcessor.before_closing_body_tag(:epub)}\s*</body>]
        assert not (content =~ ~r[#{@before_closing_head_tag_content_epub}\s*</head>])
        assert not (content =~ ~r[#{@before_closing_body_tag_content_epub}\s*</body>])
      end
    end

    # 3. Required by both the user and the markdown processor
    test "required by (1) the user and (2) the markdown processor are in the right place" do
      generate_docs_and_unzip(
        doc_config(
          before_closing_head_tag: &before_closing_head_tag/1,
          before_closing_body_tag: &before_closing_body_tag/1,
          markdown_processor: DummyProcessor
        )
      )

      oebps_dir = "#{output_dir()}/OEBPS"

      for basename <- @example_basenames do
        content = File.read!(Path.join(oebps_dir, basename))

        assert content =~
                 ~r[#{DummyProcessor.before_closing_head_tag(:epub)}\s*#{
                   @before_closing_head_tag_content_epub
                 }\s*</head>]

        assert content =~
                 ~r[#{DummyProcessor.before_closing_body_tag(:epub)}\s*#{
                   @before_closing_body_tag_content_epub
                 }\s*</body>]
      end
    end
  end

  describe "assets" do
    # There are 3 possibilities when requiring:
    # - required by the user alone
    # - required by the markdown processor alone
    # - required by both the markdown processor and the user
    # We will test the three possibilities independently

    # 1. Required by the user alone
    test "required by the user end up in the right place" do
      File.mkdir_p!("test/tmp/epub_assets/hello")
      File.touch!("test/tmp/epub_assets/hello/world.png")

      generate_docs_and_unzip(
        doc_config(assets: "test/tmp/epub_assets", logo: "test/fixtures/elixir.png")
      )

      assert File.regular?("#{output_dir()}/OEBPS/assets/hello/world.png")
      assert File.regular?("#{output_dir()}/OEBPS/assets/logo.png")
    after
      File.rm_rf!("test/tmp/epub_assets")
    end

    # 2. Required by the markdown processor
    test "required by the markdown processor end up in the right place" do
      generate_docs_and_unzip(doc_config(markdown_processor: DummyProcessor))
      # Test the assets added by the markdown processor
      for [{filename, content}] <- DummyProcessor.assets(:html) do
        # Filename matches
        assert File.regular?("#{output_dir()}/#{filename}")
        # Content matches
        assert File.read!("#{output_dir()}/#{filename}") == content
      end
    after
      File.rm_rf!("test/tmp/epub_assets")
    end

    # 3. Required by the user and markdown processor
    test "required by the user and markdown processor end up in the right place" do
      File.mkdir_p!("test/tmp/epub_assets/hello")
      File.touch!("test/tmp/epub_assets/hello/world.png")

      generate_docs_and_unzip(
        doc_config(
          markdown_processor: DummyProcessor,
          assets: "test/tmp/epub_assets",
          logo: "test/fixtures/elixir.png"
        )
      )

      assert File.regular?("#{output_dir()}/OEBPS/assets/hello/world.png")
      assert File.regular?("#{output_dir()}/OEBPS/assets/logo.png")
      # Test the assets added by the markdown processor
      for [{filename, content}] <- DummyProcessor.assets(:html) do
        # Filename matches
        assert File.regular?("#{output_dir()}/#{filename}")
        # Content matches
        assert File.read!("#{output_dir()}/#{filename}") == content
      end
    after
      File.rm_rf!("test/tmp/epub_assets")
    end
  end
end
