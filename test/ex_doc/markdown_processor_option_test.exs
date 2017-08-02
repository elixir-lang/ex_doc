defmodule ExDoc.MarkdownProcessorOptionTest do
  use ExUnit.Case

  alias ExDoc.Markdown.MockMarkdownProcessor
  alias ExDoc.Markdown.MockMarkdownProcessor.MockMarkdownProcessorError

  setup do
    File.rm_rf(output_dir())
    File.mkdir_p!(output_dir())
  end

  defp output_dir do
    Path.expand("../../tmp/html", __DIR__)
  end

  defp beam_dir do
    Path.expand("../../tmp/beam", __DIR__)
  end

  defp doc_config_markdown_processor_not_set do
    [project: "Elixir",
     version: "1.0.1",
     formatter: "html",
     assets: "test/tmp/html_assets",
     output: output_dir(),
     source_root: beam_dir(),
     source_beam: beam_dir(),
     logo: "test/fixtures/elixir.png",
     extras: ["test/fixtures/README.md"]]
  end

  defp doc_config_markdown_processor_set do
    # MockMarkdownProcessor will raise an exception if used to process files
    # This exception will be our way to tell that someting's not right.
    Keyword.put(doc_config_markdown_processor_not_set(),
                :markdown_processor,
                MockMarkdownProcessor)
  end

  def generate_docs(config) do
    # A wrapper around the child process, so that we can use assert_raise below.
    # Propagates the MockMarkdownProcessorError exception from the chid process.
    # Raises a MockMarkdownProcessorError if the exception causes the child process to exit.
    Process.flag(:trap_exit, true)
    try do
      ExDoc.generate_docs(config[:project], config[:version], config)
    catch
      # Raise the exception so that it can be captured by assert_raise
      :exit, {{%MockMarkdownProcessorError{} = exception, _}, _} -> raise exception
      # The purpose of these tests is to detect if the right markdown processor is used.
      _ -> :ok
    end
  end

  test "app env not set, markdown_processor option set" do
    Application.delete_env(:ex_doc, :markdown_processor)
    # ExDoc.Markdown should respect our choice and run the mock processor
    assert_raise MockMarkdownProcessorError, fn ->
      generate_docs doc_config_markdown_processor_set()
    end
  end

  test "app env not set, markdown_processor option not set" do
    Application.delete_env(:ex_doc, :markdown_processor)
    generate_docs doc_config_markdown_processor_not_set()
  end

  test "app env set, markdown_processor option set" do
    Application.put_env(:ex_doc, :markdown_processor, MockMarkdownProcessor)
    # It's been given two mock processors
    assert_raise MockMarkdownProcessorError, fn ->
      generate_docs doc_config_markdown_processor_set()
    end
  end

  test "app env set, markdown_processor option not set" do
    Application.put_env(:ex_doc, :markdown_processor, MockMarkdownProcessor)
    # The app env has priority
    assert_raise MockMarkdownProcessorError, fn ->
      generate_docs doc_config_markdown_processor_not_set()
    end
  end

end
