defmodule ExDoc.ConfigTest do
  use ExUnit.Case, async: true

  @project "Elixir"
  @version "1"

  defp build(opts) do
    ExDoc.Config.build(@project, @version, opts)
  end

  test "normalizes output" do
    opts_with_output = &[source_beam: "beam_dir", output: &1]

    config = build(opts_with_output.("test/tmp/ex_doc"))
    assert config.output == "test/tmp/ex_doc"

    config = build(opts_with_output.("test/tmp/ex_doc/"))
    assert config.output == "test/tmp/ex_doc"

    config = build(opts_with_output.("test/tmp/ex_doc//"))
    assert config.output == "test/tmp/ex_doc"
  end

  test "normalizes filter_modules" do
    config = build(filter_modules: "^Elixir\\.Foo")
    assert config.filter_modules.(Foo, %{})
    refute config.filter_modules.(Bar, %{})

    config = build(filter_modules: ~r/^Elixir.Foo/)
    assert config.filter_modules.(Foo, %{})
    refute config.filter_modules.(Bar, %{})

    config = build(filter_modules: fn _mod, m -> m[:works] end)
    assert config.filter_modules.(Foo, %{works: true})
    refute config.filter_modules.(Foo, %{works: false})
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

  describe "normalizes source_url_pattern" do
    test "from a function" do
      config = build(source_url_pattern: fn path, line -> "#{path}-fun-#{line}" end)
      assert config.source_url_pattern.("foo.ex", 123) == "foo.ex-fun-123"
    end

    test "from nil" do
      config = build(source_url_pattern: nil)
      assert config.source_url_pattern.("foo.ex", 123) == nil
    end

    test "from a static string" do
      config = build(source_url_pattern: "static")
      assert config.source_url_pattern.("foo.ex", 123) == "static"
    end

    test "from a %{path} only string" do
      config = build(source_url_pattern: "file:%{path}")
      assert config.source_url_pattern.("foo.ex", 123) == "file:foo.ex"

      config = build(source_url_pattern: "%{path}:file")
      assert config.source_url_pattern.("foo.ex", 123) == "foo.ex:file"

      config = build(source_url_pattern: "-%{path}-")
      assert config.source_url_pattern.("foo.ex", 123) == "-foo.ex-"
    end

    test "from a %{line} only string" do
      config = build(source_url_pattern: "line:%{line}")
      assert config.source_url_pattern.("foo.ex", 123) == "line:123"

      config = build(source_url_pattern: "%{line}:line")
      assert config.source_url_pattern.("foo.ex", 123) == "123:line"

      config = build(source_url_pattern: "-%{line}-")
      assert config.source_url_pattern.("foo.ex", 123) == "-123-"
    end

    test "from a %{path} and %{line} string" do
      config = build(source_url_pattern: "a%{path}b%{line}c")
      assert config.source_url_pattern.("foo.ex", 123) == "afoo.exb123c"

      config = build(source_url_pattern: "a%{line}b%{path}c")
      assert config.source_url_pattern.("foo.ex", 123) == "a123bfoo.exc"
    end

    test "groups_for_docs" do
      config =
        build(groups_for_docs: [Foo: &(&1[:section] == "foo"), Bar: &(&1[:section] == "bar")])

      assert config.group_for_doc.(section: "foo") == "Foo"
      assert config.group_for_doc.(section: "bar") == "Bar"
      assert config.group_for_doc.(section: "baz") == nil
      assert config.docs_groups == ~w(Foo Bar)
    end

    test "groups_for_modules" do
      # Using real applications, since we load them to extract the corresponding list of modules
      stdlib = :stdlib
      kernel = :kernel
      custom_group = :custom_group

      groups_for_modules = fn config, key ->
        List.keyfind(config.groups_for_modules, to_string(key), 0)
      end

      # Single app, no custom grouping
      config = build(apps: [stdlib])
      assert groups_for_modules.(config, stdlib) == nil
      assert groups_for_modules.(config, custom_group) == nil

      # Single app, custom grouping
      config = build(apps: [stdlib], groups_for_modules: [{"custom_group", ["module_1"]}])
      assert groups_for_modules.(config, stdlib) == nil
      assert groups_for_modules.(config, custom_group) == {"custom_group", ["module_1"]}

      # Multiple apps, no custom grouping
      config = build(apps: [stdlib, kernel])
      stdlib_groups = groups_for_modules.(config, stdlib)
      kernel_groups = groups_for_modules.(config, kernel)
      assert match?({"stdlib", _}, stdlib_groups)
      assert match?({"kernel", _}, kernel_groups)
      {"stdlib", stdlib_modules} = stdlib_groups
      {"kernel", kernel_modules} = kernel_groups
      assert Enum.member?(stdlib_modules, :gen_server)
      assert Enum.member?(kernel_modules, :file)

      # Multiple apps, custom grouping
      config = build(apps: [stdlib, kernel], groups_for_modules: [{"custom_group", ["module_1"]}])
      assert groups_for_modules.(config, stdlib) == nil
      assert groups_for_modules.(config, kernel) == nil
      assert groups_for_modules.(config, custom_group) == {"custom_group", ["module_1"]}
    end
  end

  describe "normalizes search" do
    test "defaults to local search" do
      config = build([])

      assert config.search == [
               %{name: "Default", help: "In-browser search", url: "search.html?q="}
             ]
    end

    test "accepts list of maps with name, help, and url" do
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

    test "accepts list of maps with name, help, and packages" do
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

    test "defaults url to search.html?q= when not provided" do
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

    test "raises on invalid search config" do
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
end
