defmodule ExDoc.Formatter.HtmlIOTest do
  use ExUnit.Case, async: false

  @moduletag :tmp_dir

  test "succeeds if trying to write into an empty existing directory", context do
    config = doc_config(context)

    new_output = config[:output] <> "/new-dir"
    File.mkdir_p!(new_output)

    new_config = Keyword.put(config, :output, new_output)

    refute ExUnit.CaptureIO.capture_io(:stderr, fn ->
             generate_docs(new_config)
           end) =~ "ExDoc is outputting to an existing directory"
  end

  test "warns if trying to write into existing directory with files", context do
    config = doc_config(context)
    new_output = config[:output] <> "/new-dir"

    File.mkdir_p!(new_output)
    File.touch!(Path.join(new_output, "dummy-file"))

    new_config = Keyword.put(config, :output, new_output)

    assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
             generate_docs(new_config)
           end) =~ "ExDoc is outputting to an existing directory"

    # Warn only once
    refute ExUnit.CaptureIO.capture_io(:stderr, fn ->
             generate_docs(new_config)
           end) =~ "ExDoc is outputting to an existing directory"
  end

  defp generate_docs(config) do
    config = Keyword.put_new(config, :skip_undefined_reference_warnings_on, ["Warnings"])
    ExDoc.generate_docs(config[:project], config[:version], config)
  end

  defp doc_config(%{tmp_dir: tmp_dir} = _context) do
    [
      apps: [:elixir],
      project: "Elixir",
      version: "1.0.1",
      formatter: "html",
      assets: "test/tmp/html_assets",
      output: tmp_dir <> "/html",
      source_beam: "test/tmp/beam",
      source_url: "https://github.com/elixir-lang/elixir",
      logo: "test/fixtures/elixir.png",
      extras: []
    ]
  end
end
