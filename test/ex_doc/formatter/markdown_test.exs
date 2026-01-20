defmodule ExDoc.Formatter.MarkdownTest do
  use ExUnit.Case, async: true

  @moduletag :tmp_dir

  defp config(%{tmp_dir: tmp_dir} = _context) do
    [
      project: "Elixir",
      version: "1.0.1",
      formatters: ["markdown"],
      output: tmp_dir,
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

  test "generates Markdown files in the default directory", %{tmp_dir: tmp_dir} = context do
    generate(config(context))
    assert File.regular?(tmp_dir <> "/llms.txt")
    assert File.regular?(tmp_dir <> "/api-reference.md")

    assert File.regular?(tmp_dir <> "/CompiledWithDocs.md")
    assert File.regular?(tmp_dir <> "/CompiledWithDocs.Nested.md")
    assert File.regular?(tmp_dir <> "/CustomBehaviourOne.md")
    assert File.regular?(tmp_dir <> "/CustomBehaviourTwo.md")
    assert File.regular?(tmp_dir <> "/RandomError.md")
    assert File.regular?(tmp_dir <> "/CustomProtocol.md")
    assert File.regular?(tmp_dir <> "/Mix.Tasks.TaskWithDocs.md")
  end

  test "generates module pages", %{tmp_dir: tmp_dir} = context do
    generate(config(context))

    content = File.read!(tmp_dir <> "/CompiledWithDocs.md")

    # Header
    assert content =~ "# `CompiledWithDocs`"
    assert content =~ "*example_module_tag*"

    # Moduledoc
    assert content =~ "moduledoc"

    # Function header
    assert content =~ "# `example`"

    # Function documentation
    assert content =~ "Some example"

    # Deprecated notice
    assert content =~ "> This function is deprecated. Use something else instead."

    # Struct
    assert content =~ "# `__struct__`"
    assert content =~ "Some struct"

    # Since annotation
    assert content =~ "# `example_1`"
    assert content =~ "*since 1.3.0*"

    # Macro annotation
    assert content =~ "*macro*"

    # Footer should be present when api_reference defaults to true
    assert content =~ "Consult [api-reference.md]"
  end

  test "renders types and specs properly", %{tmp_dir: tmp_dir} = context do
    generate(config(context))

    content = File.read!(tmp_dir <> "/TypesAndSpecs.md")

    # Module header
    assert content =~ "# `TypesAndSpecs`"

    # Types section - check for public type
    assert content =~ "# `public`"
    assert content =~ "A public type"

    assert content =~
             "```elixir\n@type public(t) :: {t, String.t(), TypesAndSpecs.Sub.t(), opaque(), :ok | :error}\n```"

    # Types section - check for opaque type
    assert content =~ "# `opaque`"
    assert content =~ "```elixir\n@opaque opaque()\n```"

    # Function with spec
    assert content =~ "# `add`"
    assert content =~ "```elixir\n@spec add(integer(), opaque()) :: integer()\n```"

    # Macro with spec
    assert content =~ "# `macro_spec`"
    assert content =~ "```elixir\n@spec macro_spec(any()) :: {:ok, any()}\n```"

    # Macro with spec using when clause
    assert content =~ "# `macro_with_spec`"
    assert content =~ "```elixir\n@spec macro_with_spec(v) :: {:ok, v} when v: any()\n```"
  end

  test "generates extras", %{tmp_dir: tmp_dir} = context do
    config =
      config(context,
        extras: [
          "test/fixtures/LICENSE",
          "test/fixtures/PlainText.txt",
          "test/fixtures/PlainTextFiles.md",
          "test/fixtures/README.md",
          "test/fixtures/LivebookFile.livemd",
          "test/fixtures/cheatsheets.cheatmd",
          elixir: [url: "https://elixir-lang.org"]
        ]
      )

    generate(config)
    refute File.exists?(tmp_dir <> "/elixir.md")
    assert File.exists?(tmp_dir <> "/license.md")
    assert File.exists?(tmp_dir <> "/plaintext.md")
    assert File.exists?(tmp_dir <> "/plaintextfiles.md")
    assert File.exists?(tmp_dir <> "/readme.md")
    assert File.exists?(tmp_dir <> "/livebookfile.md")
    assert File.exists?(tmp_dir <> "/cheatsheets.md")
  end

  describe "configuration options" do
    test "handles custom output directory", %{tmp_dir: tmp_dir} = context do
      custom_output = Path.join(tmp_dir, "custom_docs")
      config = config(context, output: custom_output)
      generate(config)

      assert File.regular?(custom_output <> "/llms.txt")
      assert File.regular?(custom_output <> "/CompiledWithDocs.md")
      refute File.exists?(tmp_dir <> "/llms.txt")
    end

    test "handles custom project name and version", %{tmp_dir: tmp_dir} = context do
      config = config(context, project: "MyProject", version: "2.0.0")
      generate(config)

      content = File.read!(tmp_dir <> "/llms.txt")
      assert content =~ "# MyProject v2.0.0 - Table of Contents"
    end
  end

  test "stores generated content in .build.markdown", %{tmp_dir: tmp_dir} = context do
    config = config(context, extras: ["test/fixtures/README.md"])
    generate(config)

    # Verify necessary files in .build.markdown
    content = File.read!(tmp_dir <> "/.build.markdown")
    assert content =~ "readme.md"
    assert content =~ "llms.txt"
    assert content =~ "CompiledWithDocs.md"
    assert content =~ "Mix.Tasks.TaskWithDocs.md"

    # Verify the files listed in .build.markdown actually exist
    files =
      content
      |> String.split("\n", trim: true)
      |> Enum.map(&Path.join(tmp_dir, &1))

    for file <- files do
      assert File.exists?(file)
    end
  end

  describe "llms.txt" do
    test "generates index", %{tmp_dir: tmp_dir} = context do
      config = config(context, extras: ["test/fixtures/README.md"], extra_section: "Guides")
      generate(config)

      content = File.read!(tmp_dir <> "/llms.txt")

      assert content =~ "# Elixir v1.0.1 - Table of Contents"

      assert content =~ """
             ## Guides

             - [README](readme.md)
             """

      assert content =~ """
             ## Modules

             - [CallbacksNoDocs](CallbacksNoDocs.md)
             - [Common.Nesting.Prefix.B.A](Common.Nesting.Prefix.B.A.md): moduledoc
             """

      assert content =~ """
             - Deprecated
               - [Warnings](Warnings.md): moduledoc `Warnings.bar/0`

             - Exceptions
               - [RandomError](RandomError.md)
             """

      assert content =~ """
             ## Mix Tasks

             - [mix task_with_docs](Mix.Tasks.TaskWithDocs.md): Very useful task
             """
    end

    test "when no extras exist", %{tmp_dir: tmp_dir} = context do
      config = config(context)
      generate(config)
      content = File.read!(tmp_dir <> "/llms.txt")
      refute content =~ "## Pages"
      refute content =~ "## Guides"
    end

    test "includes description when provided", %{tmp_dir: tmp_dir} = context do
      config =
        config(context,
          extras: ["test/fixtures/README.md"],
          description: "A documentation generation tool for Elixir"
        )

      generate(config)
      content = File.read!(tmp_dir <> "/llms.txt")

      assert content =~ "# Elixir v1.0.1 - Table of Contents"
      assert content =~ "A documentation generation tool for Elixir"

      # Description should not appear in api-reference.md
      api_content = File.read!(tmp_dir <> "/api-reference.md")
      assert api_content =~ "# Elixir v1.0.1 - API Reference"
      refute api_content =~ "A documentation generation tool for Elixir"
    end
  end

  describe "api_reference" do
    test "when false, does not generate api-reference.md", %{tmp_dir: tmp_dir} = context do
      config = config(context, api_reference: false)
      generate(config)
      refute File.exists?(tmp_dir <> "/api-reference.md")

      # Modules should not have the footer
      content = File.read!(tmp_dir <> "/CompiledWithDocs.md")
      refute content =~ "Consult [api-reference.md]"
    end

    test "when true, generates api-reference.md", %{tmp_dir: tmp_dir} = context do
      config =
        config(context,
          extras: ["test/fixtures/README.md"],
          extra_section: "Guides",
          api_reference: true
        )

      generate(config)
      api_content = File.read!(tmp_dir <> "/api-reference.md")

      # Should have API Reference title
      assert api_content =~ "# Elixir v1.0.1 - API Reference"

      # Should NOT include extras section
      refute api_content =~ "## Guides"
      refute api_content =~ "README"

      # Should include modules section
      assert api_content =~ """
             ## Modules

             - [CallbacksNoDocs](CallbacksNoDocs.md)
             - [Common.Nesting.Prefix.B.A](Common.Nesting.Prefix.B.A.md): moduledoc
             """

      # Should include mix tasks
      assert api_content =~ """
             ## Mix Tasks

             - [mix task_with_docs](Mix.Tasks.TaskWithDocs.md): Very useful task
             """

      # Modules should have the footer
      module_content = File.read!(tmp_dir <> "/CompiledWithDocs.md")
      assert module_content =~ "---"

      assert module_content =~
               "*Consult [api-reference.md](api-reference.md) for complete listing*"
    end
  end
end
