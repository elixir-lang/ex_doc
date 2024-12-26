defmodule ExDoc.Formatter.MARKDOWNTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureIO

  alias ExDoc.Utils

  @moduletag :tmp_dir

  @before_closing_body_tag_content_md "UNIQUE:<dont-escape>&copy;BEFORE-CLOSING-BODY-TAG-HTML</dont-escape>"

  defp before_closing_body_tag(:markdown), do: @before_closing_body_tag_content_md

  def before_closing_body_tag(:markdown, name), do: "#{name}"

  defp doc_config(%{tmp_dir: tmp_dir} = _context) do
    [
      app: :elixir,
      project: "Elixir",
      version: "1.0.1",
      formatter: "markdown",
      output: tmp_dir <> "/markdown",
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

  defp generate_docs(_context, config) do
    generate_docs(config)
  end


  test "generates a markdown nav file in the default directory", %{tmp_dir: tmp_dir} = context do
    generate_docs(doc_config(context))
    assert File.regular?(tmp_dir <> "/markdown/Elixir/MD/nav.md")
  end

  test "generates a markdown file with erlang as proglang", %{tmp_dir: tmp_dir} = context do
    config =
      context
      |> doc_config()
      |> Keyword.put(:proglang, :erlang)
      |> Keyword.update!(:skip_undefined_reference_warnings_on, &["test/fixtures/README.md" | &1])

    generate_docs(config)
    assert File.regular?(tmp_dir <> "/markdown/Elixir/MD/nav.md")
  end

  test "generates a markdown file in specified output directory", %{tmp_dir: tmp_dir} = context do
    config = doc_config(context, output: tmp_dir <> "/markdown/another_dir", main: "RandomError")
    generate_docs(config)

    assert File.regular?(tmp_dir <> "/markdown/another_dir/nav.md")
  end


  test "generates the readme file", %{tmp_dir: tmp_dir} = context do
    config = doc_config(context, main: "README")
    generate_docs(context, config)

    content = File.read!(tmp_dir <> "/markdown/Elixir/MD/readme.md")
    assert content =~ ~r{<title>README [^<]*</title>}
    assert content =~ ~r{<a href="RandomError.xhtml"><code(\sclass="inline")?>RandomError</code>}

    assert content =~
             ~r{<a href="CustomBehaviourImpl.xhtml#hello/1"><code(\sclass="inline")?>CustomBehaviourImpl.hello/1</code>}

    assert content =~
             ~r{<a href="TypesAndSpecs.Sub.xhtml"><code(\sclass="inline")?>TypesAndSpecs.Sub</code></a>}

    content = File.read!(tmp_dir <> "/markdown/Elixir/MD/nav.md")
    assert content =~ ~r{<li><a href="readme.xhtml">README</a></li>}
  end

  test "uses samp as highlight tag for markdown", %{tmp_dir: tmp_dir} = context do
    generate_docs(context, doc_config(context))

    assert File.read!(tmp_dir <> "/markdown/Elixir/MD/CompiledWithDocs.md") =~
             "<samp class=\"nc\">CompiledWithDocs<\/samp>"
  end

  @example_basenames [
    # "structural" pages
    "nav.md",
    "readme.md",
    # "module pages"
    "CompiledWithDocs.md",
    "CompiledWithDocs.Nested.md"
  ]

  test "before_closing_*_tags required by the user are in the right place",
       %{tmp_dir: tmp_dir} = context do
    generate_docs(
      context,
      doc_config(context,
        before_closing_body_tag: &before_closing_body_tag/1
      )
    )

    dir = tmp_dir <> "/markdown/Elixir/MD"

    for basename <- @example_basenames do
      content = File.read!(Path.join(dir, basename))
      assert content =~ ~r[#{@before_closing_body_tag_content_md}\s]
    end
  end

  test "before_closing_*_tags required by the user are in the right place using map",
       %{tmp_dir: tmp_dir} = context do
    generate_docs(
      context,
      doc_config(context,
        before_closing_body_tag: %{markdown: "<p>StaticDemo</p>"}
      )
    )

    dir = tmp_dir <> "/markdown/Elixir/MD"

    for basename <- @example_basenames do
      content = File.read!(Path.join(dir, basename))
      assert content =~ ~r[<p>StaticDemo</p>\s]
    end
  end

  test "before_closing_*_tags required by the user are in the right place using a MFA",
       %{tmp_dir: tmp_dir} = context do
    generate_docs(
      context,
      doc_config(context,
        before_closing_body_tag: {__MODULE__, :before_closing_body_tag, ["Demo"]}
      )
    )

    dir = tmp_dir <> "/markdown/Elixir/MD"

    for basename <- @example_basenames do
      content = File.read!(Path.join(dir, basename))
      assert content =~ ~r[<p>Demo</p>\s]
    end
  end

  test "assets required by the user end up in the right place", %{tmp_dir: tmp_dir} = context do
    File.mkdir_p!("test/tmp/markdown_assets/hello")
    File.touch!("test/tmp/markdown_assets/hello/world.png")
    File.touch!("test/tmp/markdown_assets/hello/world.pdf")

    generate_docs(
      context,
      doc_config(context,
        assets: %{"test/tmp/markdown_assets" => "assets"},
        logo: "test/fixtures/elixir.png",
        cover: "test/fixtures/elixir.png"
      )
    )

    assert File.regular?(tmp_dir <> "/markdown/assets/hello/world.png")
    assert File.regular?(tmp_dir <> "/markdown/assets/hello/world.pdf")
    assert File.regular?(tmp_dir <> "/markdown/assets/logo.png")
    assert File.regular?(tmp_dir <> "/markdown/assets/cover.png")
  after
    File.rm_rf!("test/tmp/markdown_assets")
  end

end
