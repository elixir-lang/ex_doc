defmodule ExDoc.Formatter.MarkdownTest do
  use ExUnit.Case, async: false

  @moduletag :tmp_dir

  defp doc_config(%{tmp_dir: tmp_dir} = _context) do
    [
      project: "Elixir",
      version: "1.0.1",
      formatter: "markdown",
      output: tmp_dir,
      source_beam: "test/tmp/beam",
      skip_undefined_reference_warnings_on: ["Warnings"]
    ]
  end

  defp doc_config(context, config) when is_map(context) and is_list(config) do
    Keyword.merge(doc_config(context), config)
  end

  defp generate_docs(config) do
    ExDoc.generate_docs(config[:project], config[:version], config)
  end

  test "generates markdown files in the default directory", %{tmp_dir: tmp_dir} = context do
    generate_docs(doc_config(context))
    assert File.regular?(tmp_dir <> "/markdown/index.md")
    assert File.regular?(tmp_dir <> "/markdown/CompiledWithDocs.md")
  end

  test "generates headers for module pages", %{tmp_dir: tmp_dir} = context do
    generate_docs(doc_config(context, main: "RandomError"))

    content = File.read!(tmp_dir <> "/markdown/RandomError.md")
    assert content =~ ~r{^# RandomError}m
    assert content =~ ~r{\(Elixir v1\.0\.1\)}
  end

  test "generates all listing files", %{tmp_dir: tmp_dir} = context do
    generate_docs(doc_config(context))

    assert File.regular?(tmp_dir <> "/markdown/CompiledWithDocs.md")
    assert File.regular?(tmp_dir <> "/markdown/CompiledWithDocs.Nested.md")
    assert File.regular?(tmp_dir <> "/markdown/CustomBehaviourOne.md")
    assert File.regular?(tmp_dir <> "/markdown/CustomBehaviourTwo.md")
    assert File.regular?(tmp_dir <> "/markdown/RandomError.md")
    assert File.regular?(tmp_dir <> "/markdown/CustomProtocol.md")
    assert File.regular?(tmp_dir <> "/markdown/Mix.Tasks.TaskWithDocs.md")
  end

  test "generates the index file", %{tmp_dir: tmp_dir} = context do
    generate_docs(doc_config(context))

    content = File.read!(tmp_dir <> "/markdown/index.md")
    assert content =~ ~r{^# Elixir v1\.0\.1 - Documentation - Table of Contents$}m
    assert content =~ ~r{## Modules}
    assert content =~ ~r{- \[CompiledWithDocs\]\(CompiledWithDocs\.md\)}
    assert content =~ ~r{- \[CompiledWithDocs\.Nested\]\(CompiledWithDocs\.Nested\.md\)}
  end

  test "generates module with proper structure", %{tmp_dir: tmp_dir} = context do
    generate_docs(doc_config(context))

    content = File.read!(tmp_dir <> "/markdown/CompiledWithDocs.md")

    # Header
    assert content =~ ~r{^# CompiledWithDocs  \(Elixir v1\.0\.1\)}m
    assert content =~ ~r{\*\(example_module_tag\)\*}

    # Moduledoc
    assert content =~ ~r{moduledoc}

    # Table of Contents
    assert content =~ ~r{## Table of Contents}
    assert content =~ ~r{### Functions}

    # Contents section
    assert content =~ ~r{## Contents}
  end

  test "generates functions correctly", %{tmp_dir: tmp_dir} = context do
    generate_docs(doc_config(context))

    content = File.read!(tmp_dir <> "/markdown/CompiledWithDocs.md")

    # Function in ToC
    assert content =~ ~r{####.*\[`example\(foo, bar}
    assert content =~ ~r{#example-2\)}

    # Function details
    assert content =~ ~r{<a id="example-2"></a>}
    assert content =~ ~r{#### `example\(foo, bar \\\\ Baz\)`}
    assert content =~ ~r{Some example}

    # Deprecated notice
    assert content =~ ~r{> This function is deprecated\. Use something else instead\.}

    # Struct
    assert content =~ ~r{`%CompiledWithDocs\{\}`}
    assert content =~ ~r{Some struct}

    # Since annotation
    assert content =~ ~r{example_1\(\)}
    assert content =~ ~r{\(since 1\.3\.0\)}

    # Macro annotation
    assert content =~ ~r{\(macro\)}
  end

  describe "generates extras" do
    test "ignores any external url extras", %{tmp_dir: tmp_dir} = context do
      config =
        context
        |> doc_config()
        |> Keyword.put(:extras, elixir: [url: "https://elixir-lang.org"])

      generate_docs(config)

      refute File.exists?(tmp_dir <> "/markdown/elixir.md")
    end
  end

  test "generates LLM index file", %{tmp_dir: tmp_dir} = context do
    generate_docs(doc_config(context))

    content = File.read!(tmp_dir <> "/markdown/llms.txt")

    assert content =~ ~r{# Elixir 1\.0\.1}
    assert content =~ ~r{Elixir documentation index for Large Language Models}
    assert content =~ ~r{## Modules}
    assert content =~ ~r{- \*\*CompiledWithDocs\*\* \(CompiledWithDocs\.md\):}
  end
end
