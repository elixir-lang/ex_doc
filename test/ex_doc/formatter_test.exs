defmodule ExDoc.FormatterTest do
  use ExUnit.Case, async: true

  @moduletag :tmp_dir

  # Test formatter that uses config.main to determine what files to create
  defmodule TestFormatter do
    def run(config, _modules, _extras) do
      File.mkdir_p!(config.output)

      case config.main do
        "scenario_a" ->
          File.write!(Path.join(config.output, "file1.txt"), "content1")
          File.write!(Path.join(config.output, "file2.txt"), "content2")
          %{entrypoint: "index_a.html", build: ["file1.txt", "file2.txt"]}

        "scenario_b" ->
          File.write!(Path.join(config.output, "file2.txt"), "content2_updated")
          File.write!(Path.join(config.output, "file3.txt"), "content3")
          %{entrypoint: "index_b.html", build: ["file2.txt", "file3.txt"]}

        "no_build" ->
          File.write!(Path.join(config.output, "file4.txt"), "content4")
          %{entrypoint: "index.html"}

        _ ->
          %{entrypoint: "index.html", build: []}
      end
    end
  end

  # Another test formatter to verify different build file extensions
  defmodule AnotherFormatter do
    def run(config, _modules, _extras) do
      File.mkdir_p!(config.output)
      File.write!(Path.join(config.output, "output.txt"), "test")
      %{entrypoint: "output.txt", build: ["output.txt"]}
    end
  end

  defp formatter_config(%{tmp_dir: tmp_dir}, opts \\ []) do
    %ExDoc.Formatter.Config{
      output: Path.join(tmp_dir, "output"),
      main: Keyword.get(opts, :main, "index"),
      project: "TestProject"
    }
  end

  test "creates build file on first run and cleans up on subsequent runs",
       %{tmp_dir: tmp_dir} =
         context do
    build_file = Path.join(tmp_dir, "output/.build.testformatter")

    # First run - scenario_a creates file1.txt and file2.txt
    config = formatter_config(context, main: "scenario_a")
    result = ExDoc.Formatter.run(TestFormatter, config, [], [], [])

    assert result == "index_a.html"
    assert File.exists?(Path.join(tmp_dir, "output/file1.txt"))
    assert File.exists?(Path.join(tmp_dir, "output/file2.txt"))
    refute File.exists?(Path.join(tmp_dir, "output/file3.txt"))

    # Verify build file was created with correct content
    assert File.exists?(build_file)
    content = File.read!(build_file)
    assert content =~ "file1.txt"
    assert content =~ "file2.txt"

    # Second run - scenario_b removes file1.txt, updates file2.txt, adds file3.txt
    config = formatter_config(context, main: "scenario_b")
    result = ExDoc.Formatter.run(TestFormatter, config, [], [], [])

    assert result == "index_b.html"
    refute File.exists?(Path.join(tmp_dir, "output/file1.txt"))
    assert File.exists?(Path.join(tmp_dir, "output/file2.txt"))
    assert File.exists?(Path.join(tmp_dir, "output/file3.txt"))

    # Verify build file was updated
    content = File.read!(build_file)
    refute content =~ "file1.txt"
    assert content =~ "file2.txt"
    assert content =~ "file3.txt"
  end

  test "does not delete files not listed in build file", %{tmp_dir: tmp_dir} = context do
    config = formatter_config(context, main: "scenario_a")

    # First run
    ExDoc.Formatter.run(TestFormatter, config, [], [], [])

    # Create a file that's not in the build list
    keep_file = Path.join(tmp_dir, "output/keep.txt")
    File.write!(keep_file, "keep this")

    # Second run
    ExDoc.Formatter.run(TestFormatter, config, [], [], [])

    # The keep file should still exist
    assert File.exists?(keep_file)
    assert File.read!(keep_file) == "keep this"

    # Verify it's not in the build file
    build_file = Path.join(tmp_dir, "output/.build.testformatter")
    content = File.read!(build_file)
    refute content =~ "keep.txt"
  end

  test "build file is sorted and unique", %{tmp_dir: tmp_dir} = context do
    defmodule DuplicateFormatter do
      def run(config, _modules, _extras) do
        File.mkdir_p!(config.output)
        File.write!(Path.join(config.output, "a.txt"), "a")
        File.write!(Path.join(config.output, "b.txt"), "b")
        File.write!(Path.join(config.output, "c.txt"), "c")

        # Return duplicates and unsorted
        %{entrypoint: "index.html", build: ["c.txt", "a.txt", "b.txt", "a.txt", "c.txt"]}
      end
    end

    config = formatter_config(context)
    ExDoc.Formatter.run(DuplicateFormatter, config, [], [], [])

    build_file = Path.join(tmp_dir, "output/.build.duplicateformatter")
    content = File.read!(build_file)

    # Should be sorted and unique
    lines = String.split(content, "\n", trim: true)
    assert lines == ["a.txt", "b.txt", "c.txt"]
  end
end
