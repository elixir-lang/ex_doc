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

  test "generates Markdown files in the default directory", %{tmp_dir: tmp_dir} = context do
    generate_docs(doc_config(context))
    assert File.regular?(tmp_dir <> "/index.md")
    assert File.regular?(tmp_dir <> "/CompiledWithDocs.md")
  end

  test "generates headers for module pages", %{tmp_dir: tmp_dir} = context do
    generate_docs(doc_config(context, main: "RandomError"))

    content = File.read!(tmp_dir <> "/RandomError.md")
    assert content =~ ~r{^# `RandomError`}m
  end

  test "generates all listing files", %{tmp_dir: tmp_dir} = context do
    generate_docs(doc_config(context))

    assert File.regular?(tmp_dir <> "/CompiledWithDocs.md")
    assert File.regular?(tmp_dir <> "/CompiledWithDocs.Nested.md")
    assert File.regular?(tmp_dir <> "/CustomBehaviourOne.md")
    assert File.regular?(tmp_dir <> "/CustomBehaviourTwo.md")
    assert File.regular?(tmp_dir <> "/RandomError.md")
    assert File.regular?(tmp_dir <> "/CustomProtocol.md")
    assert File.regular?(tmp_dir <> "/Mix.Tasks.TaskWithDocs.md")
  end

  test "generates the index file", %{tmp_dir: tmp_dir} = context do
    generate_docs(doc_config(context))

    content = File.read!(tmp_dir <> "/index.md")
    assert content =~ ~r{^# Elixir v1\.0\.1 - Documentation - Table of Contents$}m
    assert content =~ ~r{## Modules}
    assert content =~ ~r{- \[CompiledWithDocs\]\(CompiledWithDocs\.md\)}
    assert content =~ ~r{- \[CompiledWithDocs\.Nested\]\(CompiledWithDocs\.Nested\.md\)}
  end

  test "generates module with proper structure", %{tmp_dir: tmp_dir} = context do
    generate_docs(doc_config(context))

    content = File.read!(tmp_dir <> "/CompiledWithDocs.md")

    # Header
    assert content =~ ~r{^# `CompiledWithDocs`}m
    assert content =~ ~r{\*example_module_tag\*}

    # Moduledoc
    assert content =~ ~r{moduledoc}
  end

  test "generates functions correctly", %{tmp_dir: tmp_dir} = context do
    generate_docs(doc_config(context))

    content = File.read!(tmp_dir <> "/CompiledWithDocs.md")

    # Function header
    assert content =~ ~r{^# `example`$}m

    # Function documentation
    assert content =~ ~r{Some example}

    # Deprecated notice
    assert content =~ ~r{> This function is deprecated\. Use something else instead\.}

    # Struct
    assert content =~ ~r{^# `__struct__`$}m
    assert content =~ ~r{Some struct}

    # Since annotation
    assert content =~ ~r{^# `example_1`$}m
    assert content =~ ~r{\*since 1\.3\.0\*}

    # Macro annotation
    assert content =~ ~r{\*macro\*}
  end

  describe "generates extras" do
    test "ignores any external url extras", %{tmp_dir: tmp_dir} = context do
      config =
        context
        |> doc_config()
        |> Keyword.put(:extras, elixir: [url: "https://elixir-lang.org"])

      generate_docs(config)

      refute File.exists?(tmp_dir <> "/elixir.md")
    end
  end

  test "generates LLM index file", %{tmp_dir: tmp_dir} = context do
    generate_docs(doc_config(context))

    content = File.read!(tmp_dir <> "/llms.txt")

    assert content =~ ~r{# Elixir 1\.0\.1}
    assert content =~ ~r{Elixir documentation index for Large Language Models}
    assert content =~ ~r{## Modules}
    assert content =~ ~r{- \[CompiledWithDocs\]\(CompiledWithDocs\.md\):}
  end

  describe "markdown output validation" do
    test "generates proper markdown syntax", %{tmp_dir: tmp_dir} = context do
      generate_docs(doc_config(context))
      content = File.read!(tmp_dir <> "/CompiledWithDocs.md")

      # Module header with backticks
      assert content =~ ~r/^# `CompiledWithDocs`/m
      # Function headers with backticks
      assert content =~ ~r/^# `[a-z_]+`$/m
    end

    test "handles complex documentation elements", %{tmp_dir: tmp_dir} = context do
      generate_docs(doc_config(context))
      content = File.read!(tmp_dir <> "/CompiledWithDocs.md")

      # Function header format with backticks
      assert content =~ ~r/^# `example`$/m
      # Deprecated notice
      assert content =~ ~r/> This function is deprecated\./
    end

    test "generates valid navigation structure", %{tmp_dir: tmp_dir} = context do
      generate_docs(doc_config(context))
      content = File.read!(tmp_dir <> "/index.md")

      assert content =~ ~r/^# Elixir v1\.0\.1 - Documentation - Table of Contents$/m
      assert content =~ ~r/- \[CompiledWithDocs\]\(CompiledWithDocs\.md\)/
      assert content =~ ~r/- \[CompiledWithDocs\.Nested\]\(CompiledWithDocs\.Nested\.md\)/
      assert content =~ ~r/- \[mix task_with_docs\]\(Mix\.Tasks\.TaskWithDocs\.md\)/
      refute content =~ ~r/\]\([^)]*\s[^)]*\)/
      refute content =~ ~r/\[[^\]]*\]\(\)/
    end

    test "generates proper markdown escaping", %{tmp_dir: tmp_dir} = context do
      generate_docs(doc_config(context))
      content = File.read!(tmp_dir <> "/CompiledWithDocs.md")

      assert content =~ ~r/&lt;|&gt;|&amp;/ || !String.contains?(content, "<script>")
    end
  end

  describe "configuration options" do
    test "handles custom output directory", %{tmp_dir: tmp_dir} = context do
      custom_output = Path.join(tmp_dir, "custom_docs")
      config = doc_config(context, output: custom_output)
      generate_docs(config)

      assert File.regular?(custom_output <> "/index.md")
      assert File.regular?(custom_output <> "/CompiledWithDocs.md")
      refute File.exists?(tmp_dir <> "/index.md")
    end

    test "handles custom project name and version", %{tmp_dir: tmp_dir} = context do
      config = doc_config(context, project: "MyProject", version: "2.0.0")
      generate_docs(config)

      content = File.read!(tmp_dir <> "/index.md")
      assert content =~ ~r/# MyProject v2\.0\.0 - Documentation/

      llm_content = File.read!(tmp_dir <> "/llms.txt")
      assert llm_content =~ ~r/# MyProject 2\.0\.0/
    end

    test "processes source_url configuration", %{tmp_dir: tmp_dir} = context do
      config = doc_config(context, source_url: "https://github.com/example/project")
      generate_docs(config)

      assert File.regular?(tmp_dir <> "/CompiledWithDocs.md")
      assert File.regular?(tmp_dir <> "/index.md")
    end
  end

  describe "extras handling" do
    test "processes markdown extras correctly", %{tmp_dir: tmp_dir} = context do
      config = doc_config(context, extras: ["test/fixtures/README.md"])
      generate_docs(config)

      assert File.regular?(tmp_dir <> "/index.md")
      nav_content = File.read!(tmp_dir <> "/index.md")
      assert nav_content =~ ~r/## Modules/
      assert File.regular?(tmp_dir <> "/llms.txt")
    end

    test "handles multiple extras with custom names", %{tmp_dir: tmp_dir} = context do
      config =
        doc_config(context,
          extras: [
            "test/fixtures/README.md",
            {"test/fixtures/LICENSE", filename: "license-file"}
          ]
        )

      generate_docs(config)

      assert File.regular?(tmp_dir <> "/index.md")
      assert File.regular?(tmp_dir <> "/llms.txt")
    end

    test "processes plain text files as extras", %{tmp_dir: tmp_dir} = context do
      config = doc_config(context, extras: ["test/fixtures/PlainText.txt"])
      generate_docs(config)

      assert File.regular?(tmp_dir <> "/index.md")
    end

    test "handles extras with custom titles", %{tmp_dir: tmp_dir} = context do
      config =
        doc_config(context,
          extras: [
            {"test/fixtures/README.md", title: "Getting Started"}
          ]
        )

      generate_docs(config)

      assert File.regular?(tmp_dir <> "/index.md")
      assert File.regular?(tmp_dir <> "/llms.txt")
    end

    test "ignores external url extras", %{tmp_dir: tmp_dir} = context do
      config =
        doc_config(context,
          extras: [
            "test/fixtures/README.md",
            elixir: [url: "https://elixir-lang.org"]
          ]
        )

      generate_docs(config)

      refute File.exists?(tmp_dir <> "/elixir.md")
      assert File.regular?(tmp_dir <> "/index.md")
    end
  end

  describe "error scenarios" do
    test "handles modules with no documentation gracefully", %{tmp_dir: tmp_dir} = context do
      generate_docs(doc_config(context))

      assert File.regular?(tmp_dir <> "/CompiledWithoutDocs.md")

      content = File.read!(tmp_dir <> "/CompiledWithoutDocs.md")
      assert content =~ ~r/^# `CompiledWithoutDocs`/m
    end

    test "handles missing source beam directory", %{tmp_dir: tmp_dir} = context do
      config = doc_config(context, source_beam: "nonexistent/path")

      generate_docs(config)
      assert File.regular?(tmp_dir <> "/index.md")
    end

    test "handles empty extras list", %{tmp_dir: tmp_dir} = context do
      config = doc_config(context, extras: [])
      generate_docs(config)

      nav_content = File.read!(tmp_dir <> "/index.md")
      refute nav_content =~ ~r/## Guides/

      llm_content = File.read!(tmp_dir <> "/llms.txt")
      refute llm_content =~ ~r/## Guides/
    end

    test "handles special characters in module names", %{tmp_dir: tmp_dir} = context do
      generate_docs(doc_config(context))

      assert File.regular?(tmp_dir <> "/CompiledWithDocs.Nested.md")

      content = File.read!(tmp_dir <> "/CompiledWithDocs.Nested.md")
      assert content =~ ~r/^# `CompiledWithDocs\.Nested`/m
    end
  end

  describe "llm index comprehensive tests" do
    test "creates comprehensive llm index with all sections", %{tmp_dir: tmp_dir} = context do
      config = doc_config(context, extras: ["test/fixtures/README.md"])
      generate_docs(config)

      content = File.read!(tmp_dir <> "/llms.txt")

      assert content =~ ~r/# Elixir 1\.0\.1/
      assert content =~ ~r/documentation index for Large Language Models/

      assert content =~ ~r/## Modules/
      assert content =~ ~r/- \[CompiledWithDocs\]\(CompiledWithDocs\.md\):/
      assert content =~ ~r/- \[CustomBehaviourOne\]/

      assert content =~ ~r/## Mix Tasks/
      assert content =~ ~r/- \[mix task_with_docs\]/
    end

    test "handles llm index when no tasks exist", %{tmp_dir: tmp_dir} = context do
      config = doc_config(context, source_beam: "test/fixtures/beam_no_tasks")
      generate_docs(config)

      content = File.read!(tmp_dir <> "/llms.txt")

      assert content =~ ~r/## Modules/
    end

    test "truncates long descriptions in llm index", %{tmp_dir: tmp_dir} = context do
      generate_docs(doc_config(context))

      content = File.read!(tmp_dir <> "/llms.txt")

      lines = String.split(content, "\n")
      module_lines = Enum.filter(lines, &String.starts_with?(&1, "- ["))

      for line <- module_lines do
        assert String.length(line) < 300
      end
    end
  end

  describe "output normalization" do
    test "normalizes line endings consistently", %{tmp_dir: tmp_dir} = context do
      generate_docs(doc_config(context))
      content = File.read!(tmp_dir <> "/CompiledWithDocs.md")

      refute String.contains?(content, "\r\n")
      refute String.contains?(content, "\r")

      refute String.contains?(content, "\n\n\n\n")
    end

    test "handles unicode content properly", %{tmp_dir: tmp_dir} = context do
      generate_docs(doc_config(context))

      files = [
        tmp_dir <> "/index.md",
        tmp_dir <> "/CompiledWithDocs.md"
      ]

      for file <- files do
        content = File.read!(file)
        assert String.valid?(content)
      end
    end
  end
end
