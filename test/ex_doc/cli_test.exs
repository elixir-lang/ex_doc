defmodule ExDoc.CLITest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureIO

  @ebin "_build/test/lib/ex_doc/ebin"

  defp run(args) do
    # TODO: Use with_io on Elixir v1.13
    io =
      capture_io(fn ->
        send(self(), ExDoc.CLI.main(args, &{&1, &2, &3}))
      end)

    assert_receive response
    {response, io}
  end

  test "minimum command-line options" do
    {[html, epub], _io} = run(["ExDoc", "1.2.3", @ebin])
    assert html == {"ExDoc", "1.2.3", [formatter: "html", apps: [:ex_doc], source_beam: @ebin]}
    assert epub == {"ExDoc", "1.2.3", [formatter: "epub", apps: [:ex_doc], source_beam: @ebin]}
  end

  test "formatter option" do
    {[epub, html], _io} = run(["ExDoc", "1.2.3", @ebin, "-f", "epub", "-f", "html"])
    assert epub == {"ExDoc", "1.2.3", [formatter: "epub", apps: [:ex_doc], source_beam: @ebin]}
    assert html == {"ExDoc", "1.2.3", [formatter: "html", apps: [:ex_doc], source_beam: @ebin]}
  end

  test "version" do
    {_, io} = run(["--version"])
    assert io == "ExDoc v#{ExDoc.version()}\n"

    {_, io} = run(["--version"])
    assert io == "ExDoc v#{ExDoc.version()}\n"
  end

  test "too many arguments" do
    assert catch_exit(run(["ExDoc", "1.2.3", "/", "kaboom"])) == {:shutdown, 1}
  end

  test "too few arguments" do
    assert catch_exit(run(["ExDoc"])) == {:shutdown, 1}
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

    {[{project, version, opts}], _io} = run(args)
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

  test "with --quiet" do
    {_, io} = run(["ExDoc", "1.2.3", @ebin, "-q"])
    assert io == ""
  end

  describe "--config .exs" do
    test "loading" do
      File.write!("test.exs", ~s([extras: ["README.md"], formatters: ["html"]]))

      {[{project, version, opts}], _io} =
        run(["ExDoc", "--extra-section", "Guides", "1.2.3", @ebin, "-c", "test.exs"])

      assert project == "ExDoc"
      assert version == "1.2.3"

      assert Enum.sort(opts) == [
               apps: [:ex_doc],
               extra_section: "Guides",
               extras: ["README.md"],
               formatter: "html",
               formatters: ["html"],
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
      File.write!("test.config", ~s({extras, [<<"README.md">>]}. {formatters, [<<"html">>]}.))

      {[{project, version, opts}], _io} = run(["ExDoc", "1.2.3", @ebin, "-c", "test.config"])

      assert project == "ExDoc"
      assert version == "1.2.3"

      assert Enum.sort(opts) == [
               apps: [:ex_doc],
               extras: ["README.md"],
               formatter: "html",
               formatters: ["html"],
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
