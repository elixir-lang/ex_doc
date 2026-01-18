defmodule ExDoc.Formatter.ConfigTest do
  use ExUnit.Case, async: true

  @project "Elixir"
  @version "1"

  defp build(opts) do
    ExDoc.Formatter.Config.build(@project, @version, opts)
  end

  test "normalizes output" do
    opts_with_output = &[source_beam: "beam_dir", output: &1]

    config = build(opts_with_output.("test/tmp/ex_doc"))
    assert config.output == Path.expand("test/tmp/ex_doc")

    config = build(opts_with_output.("test/tmp/ex_doc/"))
    assert config.output == Path.expand("test/tmp/ex_doc")

    config = build(opts_with_output.("test/tmp/ex_doc//"))
    assert config.output == Path.expand("test/tmp/ex_doc")
  end

  test "normalizes skip_undefined_reference_warnings_on" do
    config = build(skip_undefined_reference_warnings_on: ["Foo", "Bar.baz/0"])

    assert config.skip_undefined_reference_warnings_on.("Foo")
    assert config.skip_undefined_reference_warnings_on.("Bar.baz/0")
    refute config.skip_undefined_reference_warnings_on.("Foo.bar/1")

    config =
      build(skip_undefined_reference_warnings_on: &String.match?(&1, ~r/Foo/))

    assert config.skip_undefined_reference_warnings_on.("Foo")
    refute config.skip_undefined_reference_warnings_on.("Bar.baz/0")
    assert config.skip_undefined_reference_warnings_on.("Foo.bar/1")
  end

  test "normalizes skip_code_autolink_to" do
    config = build(skip_code_autolink_to: ["ConfigTest.Hidden", "ConfigTest.Hidden.foo/1"])

    assert config.skip_code_autolink_to.("ConfigTest.Hidden")
    assert config.skip_code_autolink_to.("ConfigTest.Hidden.foo/1")
    refute config.skip_code_autolink_to.("ConfigTest.Hidden.foo/2")
    refute config.skip_code_autolink_to.("ConfigTest.Hidden.bar/1")
    refute config.skip_code_autolink_to.("ConfigTest.NotHidden")

    config = build(skip_code_autolink_to: &String.match?(&1, ~r/\AConfigTest\.Hidden/))

    assert config.skip_code_autolink_to.("ConfigTest.Hidden")
    assert config.skip_code_autolink_to.("ConfigTest.Hidden.foo/1")
    assert config.skip_code_autolink_to.("ConfigTest.Hidden.foo/2")
    assert config.skip_code_autolink_to.("ConfigTest.Hidden.bar/1")
    refute config.skip_code_autolink_to.("ConfigTest.NotHidden")
  end

  test "normalizes search: defaults to local search" do
    config = build([])

    assert config.search == [
             %{name: "Default", help: "In-browser search", url: "search.html?q="}
           ]
  end

  test "normalizes search: accepts list of maps with name, help, and url" do
    config =
      build(
        search: [
          %{name: "Google", help: "Search using Google", url: "https://google.com/?q="},
          %{name: "Local", help: "Search locally", url: "search.html?q="}
        ]
      )

    assert config.search == [
             %{name: "Google", help: "Search using Google", url: "https://google.com/?q="},
             %{name: "Local", help: "Search locally", url: "search.html?q="}
           ]
  end

  test "normalizes search: accepts list of maps with name, help, and packages" do
    config =
      build(
        search: [
          %{name: "Local", help: "Search locally", packages: [:ex_doc, elixir: "main"]}
        ]
      )

    assert config.search == [
             %{
               name: "Local",
               help: "Search locally",
               url: "https://hexdocs.pm/?packages=ex_doc%3Alatest%2Celixir%3Amain&q="
             }
           ]
  end

  test "normalizes search: defaults url to search.html?q= when not provided" do
    config =
      build(
        search: [
          %{name: "Default", help: "In-browser search"}
        ]
      )

    assert config.search == [
             %{name: "Default", help: "In-browser search", url: "search.html?q="}
           ]
  end

  test "normalizes search: raises on invalid search config" do
    assert_raise ArgumentError, ~r/search must be a list of maps/, fn ->
      build(search: "invalid")
    end

    assert_raise ArgumentError, ~r/search entries must be a map/, fn ->
      build(search: [%{name: "Test"}])
    end

    assert_raise ArgumentError, ~r/search entries must be a map/, fn ->
      build(search: [%{name: "Test", help: "Help", url: 123}])
    end
  end
end
