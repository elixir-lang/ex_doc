defmodule ExDoc.Formatter.HtmlIOTest do
  use ExUnit.Case, async: false

  @moduletag :tmp_dir

  test "succeeds if trying to write into an empty existing directory", %{tmp_dir: tmp_dir} do
    File.mkdir!("#{tmp_dir}/doc")

    assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
             generate_docs(tmp_dir)
           end) =~ ""
  end

  test "warns if trying to write into existing directory with files", %{tmp_dir: tmp_dir} do
    File.mkdir!("#{tmp_dir}/doc")
    File.touch!("#{tmp_dir}/doc/foo.txt")

    assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
             generate_docs(tmp_dir)
           end) =~ "ExDoc is outputting to an existing directory"

    # Warn only once
    assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
             generate_docs(tmp_dir)
           end) =~ ""
  end

  defp generate_docs(tmp_dir) do
    config = [
      app: :foo,
      formatter: "html",
      output: "#{tmp_dir}/doc",
      source_beam: "#{tmp_dir}/ebin",
    ]

    ExDoc.generate_docs("Foo", "1.0.0", config)
  end
end
