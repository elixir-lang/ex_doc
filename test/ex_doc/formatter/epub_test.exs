defmodule ExDoc.Formatter.EPUBTest do
  use ExUnit.Case, async: true

  require Record
  Record.defrecordp(:xmlElement, Record.extract(:xmlElement, from_lib: "xmerl/include/xmerl.hrl"))

  @moduletag :tmp_dir
  @before_closing_head_tag_content_epub "UNIQUE:<dont-escape>&copy;BEFORE-CLOSING-HEAD-TAG-HTML</dont-escape>"
  @before_closing_body_tag_content_epub "UNIQUE:<dont-escape>&copy;BEFORE-CLOSING-BODY-TAG-HTML</dont-escape>"

  defp before_closing_head_tag(:epub), do: @before_closing_head_tag_content_epub
  defp before_closing_body_tag(:epub), do: @before_closing_body_tag_content_epub

  def before_closing_head_tag(:epub, name), do: "<meta name=#{name}>"
  def before_closing_body_tag(:epub, name), do: "<p>#{name}</p>"

  defp config(%{tmp_dir: tmp_dir} = _context) do
    [
      app: :elixir,
      project: "Elixir",
      version: "1.0.1",
      formatters: ["epub"],
      output: tmp_dir <> "/epub",
      source_beam: "test/tmp/beam",
      skip_undefined_reference_warnings_on: ["Warnings"]
    ]
  end

  defp config(context, config) when is_map(context) and is_list(config) do
    Keyword.merge(config(context), config)
  end

  defp generate(config) do
    source_beam = config[:source_beam] |> List.wrap()
    ExDoc.generate(config[:project], config[:version], source_beam, config)
  end

  defp generate_and_unzip(context, config) do
    generate(config)
    unzip_dir = String.to_charlist("#{config(context)[:output]}")

    "#{config(context)[:output]}/#{config(context)[:project]}.epub"
    |> String.to_charlist()
    |> :zip.unzip(cwd: unzip_dir)
  end

  defp read_xml(path, root) do
    content = File.read!(path)

    assert {xmlElement(name: ^root), []} =
             content
             |> :binary.bin_to_list()
             |> :xmerl_scan.string()

    content
  end

  test "generates headers for module pages", %{tmp_dir: tmp_dir} = context do
    generate_and_unzip(context, config(context, main: "RandomError"))

    content = read_xml(tmp_dir <> "/epub/OEBPS/RandomError.xhtml", :html)
    assert content =~ ~r{<html.*lang="en".*xmlns:epub="http://www.idpf.org/2007/ops">}ms
    assert content =~ ~r{<meta charset="utf-8" />}ms
    assert content =~ ~r{<meta name="generator" content="ExDoc v[^"]+" />}
    assert content =~ ~r{<title>RandomError - Elixir v1.0.1</title>}
  end

  test "allows to set the primary language of the document", %{tmp_dir: tmp_dir} = context do
    generate_and_unzip(context, config(context, main: "RandomError", language: "fr"))

    content = read_xml(tmp_dir <> "/epub/OEBPS/RandomError.xhtml", :html)
    assert content =~ ~r{<html.*lang="fr".*xmlns:epub="http://www.idpf.org/2007/ops">}ms
  end

  test "allows to set the authors of the document", %{tmp_dir: tmp_dir} = context do
    generate_and_unzip(context, config(context, authors: ["John Doe", "Jane Doe"]))

    content = read_xml(tmp_dir <> "/epub/OEBPS/content.opf", :package)
    assert content =~ ~r{<dc:creator id="author1">John Doe</dc:creator>}
    assert content =~ ~r{<dc:creator id="author2">Jane Doe</dc:creator>}
  end

  test "generates an EPUB file in the default directory", %{tmp_dir: tmp_dir} = context do
    generate(config(context))
    assert File.regular?(tmp_dir <> "/epub/#{config(context)[:project]}.epub")
  end

  test "generates an EPUB file with erlang as proglang", %{tmp_dir: tmp_dir} = context do
    config =
      context
      |> config()
      |> Keyword.put(:proglang, :erlang)
      |> Keyword.update!(:skip_undefined_reference_warnings_on, &["test/fixtures/README.md" | &1])

    generate(config)
    assert File.regular?(tmp_dir <> "/epub/#{config[:project]}.epub")
  end

  test "generates an EPUB file in specified output directory", %{tmp_dir: tmp_dir} = context do
    config = config(context, output: tmp_dir <> "/epub/another_dir", main: "RandomError")
    generate(config)

    assert File.regular?(tmp_dir <> "/epub/another_dir/#{config(context)[:project]}.epub")
  end

  test "generates an EPUB file with a standardized structure", %{tmp_dir: tmp_dir} = context do
    generate_and_unzip(context, config(context))

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
    assert File.regular?("#{oebps_dir}/CompiledWithDocs.xhtml")
    assert File.regular?("#{oebps_dir}/CompiledWithDocs.Nested.xhtml")
    assert [_] = Path.wildcard("#{dist_dir}/epub-*.js")
    assert [_] = Path.wildcard("#{dist_dir}/epub-elixir-*.css")
  end

  test "generates all listing files", %{tmp_dir: tmp_dir} = context do
    generate_and_unzip(context, config(context))
    content = read_xml(tmp_dir <> "/epub/OEBPS/content.opf", :package)

    assert content =~ ~r{.*"CompiledWithDocs\".*}ms
    assert content =~ ~r{.*"CompiledWithDocs.Nested\".*}ms
    assert content =~ ~r{.*"CustomBehaviourOne\".*}ms
    assert content =~ ~r{.*"CustomBehaviourTwo\".*}ms
    assert content =~ ~r{.*"RandomError\".*}ms
    assert content =~ ~r{.*"CustomProtocol\".*}ms
    assert content =~ ~r{.*"Mix\.Tasks\.TaskWithDocs\".*}ms
  end

  test "generates the readme file as main", %{tmp_dir: tmp_dir} = context do
    config = config(context, main: "README", extras: ["test/fixtures/README.md"])
    generate_and_unzip(context, config)

    content = read_xml(tmp_dir <> "/epub/OEBPS/nav.xhtml", :html)
    assert content =~ ~r{<li><a href="readme.xhtml">README</a></li>}
  end

  test "generates and renders extras", %{tmp_dir: tmp_dir} = context do
    config =
      config(context,
        extras: [
          {"test/fixtures/LICENSE", filename: "a&b"},
          "test/fixtures/PlainText.txt",
          "test/fixtures/PlainTextFiles.md",
          "test/fixtures/cheatsheets.cheatmd"
        ]
      )

    generate_and_unzip(context, config)

    # Markdown files are rendered with formatting and autolinks
    content = read_xml(tmp_dir <> "/epub/OEBPS/plaintextfiles.xhtml", :html)
    assert content =~ ~r{Plain Text Files</h1>}s
    assert content =~ ~r{<a href="plaintext.xhtml">plain-text file</a>}

    # Plain text files are rendered as preformatted
    plain_text_file = read_xml(tmp_dir <> "/epub/OEBPS/plaintext.xhtml", :html)
    assert plain_text_file =~ ~r{<pre>\nThis is plain\n  text and nothing\n.+\s+good bye\n</pre>}s

    # Cheatmd files have section headers with IDs
    cheatsheet = read_xml(tmp_dir <> "/epub/OEBPS/cheatsheets.xhtml", :html)
    assert cheatsheet =~ ~s{<h2 id="getting-started">}
    assert cheatsheet =~ ~s{<h3 id="hello-world">}

    manifest = read_xml(tmp_dir <> "/epub/OEBPS/content.opf", :package)
    assert manifest =~ ~s{<item id="a&amp;b" href="a&amp;b.xhtml"}
    assert manifest =~ ~s{<itemref idref="a&amp;b"/>}

    nav = read_xml(tmp_dir <> "/epub/OEBPS/nav.xhtml", :html)
    assert nav =~ ~s{<a href="a&amp;b.xhtml">}
  end

  test "ignores any external url extras", %{tmp_dir: tmp_dir} = context do
    config =
      context
      |> config()
      |> Keyword.put(:extras, elixir: [url: "https://elixir-lang.org"])

    generate_and_unzip(context, config)

    refute File.exists?(tmp_dir <> "/epub/OEBPS/elixir.xhtml")
  end

  test "uses samp as highlight tag for markdown", %{tmp_dir: tmp_dir} = context do
    generate_and_unzip(context, config(context))

    assert read_xml(tmp_dir <> "/epub/OEBPS/CompiledWithDocs.xhtml", :html) =~
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

  test "generates before_closing_*_tags", %{tmp_dir: tmp_dir} = context do
    generate_and_unzip(
      context,
      config(context,
        before_closing_head_tag: &before_closing_head_tag/1,
        before_closing_body_tag: &before_closing_body_tag/1,
        extras: ["test/fixtures/README.md"]
      )
    )

    oebps_dir = tmp_dir <> "/epub/OEBPS"

    for basename <- @example_basenames do
      content = read_xml(Path.join(oebps_dir, basename), :html)

      assert content =~
               ~r[#{ExDoc.EPUB.Entities.to_numeric(@before_closing_head_tag_content_epub)}\s*</head>]

      assert content =~
               ~r[#{ExDoc.EPUB.Entities.to_numeric(@before_closing_body_tag_content_epub)}\s*</body>]
    end
  end

  test "generates assets required by the user", %{tmp_dir: tmp_dir} = context do
    File.mkdir_p!("test/tmp/epub_assets/hello")
    File.touch!("test/tmp/epub_assets/hello/world.png")
    File.touch!("test/tmp/epub_assets/hello/world.pdf")

    generate_and_unzip(
      context,
      config(context,
        assets: %{"test/tmp/epub_assets" => "assets"},
        logo: "test/fixtures/elixir.png",
        cover: "test/fixtures/elixir.png"
      )
    )

    assert File.regular?(tmp_dir <> "/epub/OEBPS/assets/hello/world.png")
    assert File.regular?(tmp_dir <> "/epub/OEBPS/assets/hello/world.pdf")
    assert File.regular?(tmp_dir <> "/epub/OEBPS/assets/logo.png")
    assert File.regular?(tmp_dir <> "/epub/OEBPS/assets/cover.png")
  after
    File.rm_rf!("test/tmp/epub_assets")
  end

  test "stores generated EPUB file in .build.epub", %{tmp_dir: tmp_dir} = context do
    config = config(context, extras: ["test/fixtures/README.md"])
    generate(config)

    content = File.read!(tmp_dir <> "/epub/.build.epub")
    assert content =~ ~r(Elixir\.epub$)m
  end
end
