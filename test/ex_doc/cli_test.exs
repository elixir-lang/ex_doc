defmodule ExDoc.CLITest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureIO

  @ebin "_build/test/lib/ex_doc/ebin"

  defp run(args, generator \\ &{&1, &2, &3}, io_device \\ :stdio) do
    # TODO: Use with_io on Elixir v1.13
    io =
      capture_io(io_device, fn ->
        send(self(), ExDoc.CLI.main(args, generator))
      end)

    assert_receive response
    {response, io}
  end

  test "minimum command-line options" do
    {[html, epub], _io} = run(["ExDoc", "1.2.3", @ebin])

    assert html ==
             {:ok,
              {"ExDoc", "1.2.3",
               [
                 formatter: "html",
                 formatters: ["html", "epub"],
                 apps: [:ex_doc],
                 source_beam: @ebin
               ]}}

    assert epub ==
             {:ok,
              {"ExDoc", "1.2.3",
               [
                 formatter: "epub",
                 formatters: ["html", "epub"],
                 apps: [:ex_doc],
                 source_beam: @ebin
               ]}}
  end

  test "formatter option" do
    {[epub, html], _io} = run(["ExDoc", "1.2.3", @ebin, "-f", "epub", "-f", "html"])

    assert epub ==
             {:ok,
              {"ExDoc", "1.2.3",
               [
                 formatter: "epub",
                 formatters: ["epub", "html"],
                 apps: [:ex_doc],
                 source_beam: @ebin
               ]}}

    assert html ==
             {:ok,
              {"ExDoc", "1.2.3",
               [
                 formatter: "html",
                 formatters: ["epub", "html"],
                 apps: [:ex_doc],
                 source_beam: @ebin
               ]}}
  end

  test "version" do
    {_, io} = run(["--version"])
    assert io =~ "ExDoc v#{ExDoc.version()}\n"

    {_, io} = run(["--version"])
    assert io =~ "ExDoc v#{ExDoc.version()}\n"
  end

  describe "--warnings-as-errors" do
    @describetag :warnings

    test "exits with 0 when there are warnings and --warnings-as-errors flag is not set" do
      ExDoc.Utils.unset_warned()

      Mix.Project.in_project(:single, "test/fixtures/single", fn _mod ->
        source_beam = "_build/test/lib/single/ebin"

        fun = fn ->
          run(
            ["Single", "1.2.3", source_beam, "--formatter=html"],
            &ExDoc.generate_docs/3,
            :stderr
          )
        end

        {[_html], io} = fun.()

        assert io =~
                 ~s|documentation references function \"Single.bar/0\" but it is undefined or private|

        # TODO: remove check when we require Elixir v1.16
        if Version.match?(System.version(), ">= 1.16.0-rc") do
          assert io =~ ~S|moduledoc `Single.bar/0`|
          assert io =~ ~S|doc `Single.bar/0`|
        end
      end)
    end

    test "exits with 1 when there are warnings with --warnings-as-errors flag" do
      ExDoc.Utils.unset_warned()

      Mix.Project.in_project(:single, "test/fixtures/single", fn _mod ->
        source_beam = "_build/test/lib/single/ebin"

        fun = fn ->
          run(
            ["Single", "1.2.3", source_beam, "--formatter=html", "--warnings-as-errors"],
            &ExDoc.generate_docs/3,
            :stderr
          )
        end

        # fun.()

        io =
          capture_io(:stderr, fn ->
            assert catch_exit(fun.()) == {:shutdown, 1}
          end)

        assert io =~
                 "Documents have been generated, but generation for html format failed due to warnings " <>
                   "while using the --warnings-as-errors option."
      end)
    end
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

    {[{:ok, {project, version, opts}}], _io} = run(args)
    assert project == "ExDoc"
    assert version == "1.2.3"

    assert Enum.sort(opts) == [
             apps: [:ex_doc],
             canonical: "http://example.com/project",
             formatter: "html",
             formatters: ["html"],
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

      {[{:ok, {project, version, opts}}], _io} =
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

    test "switches take precedence over config" do
      File.write!("test.exs", ~s([logo: "config_logo.png", formatters: ["html"]]))

      {[{:ok, {project, version, opts}}], _io} =
        run([
          "ExDoc",
          "--logo",
          "opts_logo.png",
          "1.2.3",
          @ebin,
          "-c",
          "test.exs"
        ])

      assert project == "ExDoc"
      assert version == "1.2.3"

      assert Enum.sort(opts) == [
               apps: [:ex_doc],
               formatter: "html",
               formatters: ["html"],
               logo: "opts_logo.png",
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

      {[{:ok, {project, version, opts}}], _io} =
        run(["ExDoc", "1.2.3", @ebin, "-c", "test.config"])

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
