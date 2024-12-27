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
      output: tmp_dir,
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

  test "generates a markdown index file in the default directory",
       %{tmp_dir: tmp_dir} = context do
    generate_docs(doc_config(context))
    assert File.regular?(tmp_dir <> "/markdown/index.md")
  end

  test "generates a markdown file with erlang as proglang", %{tmp_dir: tmp_dir} = context do
    config =
      context
      |> doc_config()
      |> Keyword.put(:proglang, :erlang)
      |> Keyword.update!(:skip_undefined_reference_warnings_on, &["test/fixtures/README.md" | &1])

    generate_docs(config)
    assert File.regular?(tmp_dir <> "/markdown/index.md")
  end

  test "generates a markdown file in specified output directory", %{tmp_dir: tmp_dir} = context do
    config = doc_config(context, output: tmp_dir <> "/another_dir", main: "RandomError")
    generate_docs(config)

    assert File.regular?(tmp_dir <> "/another_dir/markdown/index.md")
  end

  test "generates the readme file", %{tmp_dir: tmp_dir} = context do
    config = doc_config(context, main: "README")
    generate_docs(context, config)

    content = File.read!(tmp_dir <> "/markdown/readme.md")
    assert content =~ ~r{`RandomError`\n}

    assert content =~
             ~r{\n`CustomBehaviourImpl.hello/1`\n}

    assert content =~
             ~r{\n`TypesAndSpecs.Sub`\n}

    content = File.read!(tmp_dir <> "/markdown/index.md")
    assert content =~ "# Table of contents\n\n  - [README](readme.md)"
  end
end
