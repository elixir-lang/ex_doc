defmodule ExDoc.ConfigTest do
  use ExUnit.Case, async: true

  @project "Elixir"
  @version "1"

  defp build(opts) do
    ExDoc.Config.build(@project, @version, opts)
    ExDoc.Config.build(@project, @version, opts)
    ExDoc.Config.build(@project, @version, opts)
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
  end
end
