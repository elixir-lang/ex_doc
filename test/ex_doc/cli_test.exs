defmodule ExDoc.CLITest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureIO

  @ebin "_build/test/lib/ex_doc/ebin"

  defp run(args) do
    ExDoc.CLI.main(args, &{&1, &2, &3})
  end

  test "minimum command-line options" do
    assert {"ExDoc", "1.2.3", [apps: [:ex_doc], source_beam: @ebin]} ==
             run(["ExDoc", "1.2.3", @ebin])
  end

  test "version" do
    assert capture_io(fn ->
             run(["--version"])
           end) == "ExDoc v#{ExDoc.version()}\n"

    assert capture_io(fn ->
             run(["-v"])
           end) == "ExDoc v#{ExDoc.version()}\n"
  end

  test "too many arguments" do
    fun = fn ->
      run(["ExDoc", "1.2.3", "/", "kaboom"])
    end

    assert catch_exit(capture_io(fun)) == {:shutdown, 1}
  end

  test "too few arguments" do
    fun = fn ->
      run(["ExDoc"])
    end

    assert catch_exit(capture_io(fun)) == {:shutdown, 1}
  end

  test "arguments that are not aliased" do
    File.write!("not_aliased.exs", ~s([key: "val"]))

    args = ~w(
      ExDoc 1.2.3 #{@ebin}
      --config not_aliased.exs
      --output html
      --formatter html
      --source-url http://example.com/username/project
      --source-ref abcdefg
      --main Main
      --homepage-url http://example.com
      --logo logo.png
      --canonical http://example.com/project
    )

    {project, version, opts} = run(args)
    assert project == "ExDoc"
    assert version == "1.2.3"

    assert Enum.sort(opts) == [
             apps: [:ex_doc],
             canonical: "http://example.com/project",
             formatter: "html",
             homepage_url: "http://example.com",
             key: "val",
             logo: "logo.png",
             main: "Main",
             output: "html",
             source_beam: "#{@ebin}",
             source_ref: "abcdefg",
             source_url: "http://example.com/username/project"
           ]
  after
    File.rm!("not_aliased.exs")
  end

  describe "--config .exs" do
    test "loading" do
      File.write!("test.exs", ~s([extras: ["README.md"]]))

      {project, version, opts} =
        run(["ExDoc", "--extra-section", "Guides", "1.2.3", @ebin, "-c", "test.exs"])

      assert project == "ExDoc"
      assert version == "1.2.3"

      assert Enum.sort(opts) == [
               apps: [:ex_doc],
               extra_section: "Guides",
               extras: ["README.md"],
               source_beam: @ebin
             ]
    after
      File.rm!("test.exs")
    end

    test "missing" do
      assert_raise File.Error, fn ->
        run(["ExDoc", "1.2.3", @ebin, "-c", "test.exs"])
      end
    end

    test "invalid" do
      File.write!("test.exs", ~s(%{"extras" => "README.md"}))

      assert_raise RuntimeError, ~S(expected a keyword list from config file: "test.exs"), fn ->
        run(["ExDoc", "1.2.3", @ebin, "-c", "test.exs"])
      end
    after
      File.rm!("test.exs")
    end
  end

  describe "--config .config" do
    test "loading" do
      File.write!("test.config", ~s({extras, [<<"README.md">>]}.))

      {project, version, opts} = run(["ExDoc", "1.2.3", @ebin, "-c", "test.config"])

      assert project == "ExDoc"
      assert version == "1.2.3"

      assert Enum.sort(opts) == [
               apps: [:ex_doc],
               extras: ["README.md"],
               source_beam: @ebin
             ]
    after
      File.rm!("test.config")
    end

    test "invalid" do
      File.write!("test.config", "bad")

      assert_raise RuntimeError, ~r/error parsing test.config/, fn ->
        run(["ExDoc", "1.2.3", @ebin, "-c", "test.config"])
      end
    after
      File.rm!("test.config")
    end
  end
end
