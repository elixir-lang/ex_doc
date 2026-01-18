defmodule ExDoc.ConfigTest do
  use ExUnit.Case, async: true

  defp build(opts) do
    ExDoc.Config.build(opts)
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

  test "normalizes source_url_pattern: from a function" do
    config = build(source_url_pattern: fn path, line -> "#{path}-fun-#{line}" end)
    assert config.source_url_pattern.("foo.ex", 123) == "foo.ex-fun-123"
  end

  test "normalizes source_url_pattern: from nil" do
    config = build(source_url_pattern: nil)
    assert config.source_url_pattern.("foo.ex", 123) == nil
  end

  test "normalizes source_url_pattern: from a static string" do
    config = build(source_url_pattern: "static")
    assert config.source_url_pattern.("foo.ex", 123) == "static"
  end

  test "normalizes source_url_pattern: from a %{path} only string" do
    config = build(source_url_pattern: "file:%{path}")
    assert config.source_url_pattern.("foo.ex", 123) == "file:foo.ex"

    config = build(source_url_pattern: "%{path}:file")
    assert config.source_url_pattern.("foo.ex", 123) == "foo.ex:file"

    config = build(source_url_pattern: "-%{path}-")
    assert config.source_url_pattern.("foo.ex", 123) == "-foo.ex-"
  end

  test "normalizes source_url_pattern: from a %{line} only string" do
    config = build(source_url_pattern: "line:%{line}")
    assert config.source_url_pattern.("foo.ex", 123) == "line:123"

    config = build(source_url_pattern: "%{line}:line")
    assert config.source_url_pattern.("foo.ex", 123) == "123:line"

    config = build(source_url_pattern: "-%{line}-")
    assert config.source_url_pattern.("foo.ex", 123) == "-123-"
  end

  test "normalizes source_url_pattern: from a %{path} and %{line} string" do
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
