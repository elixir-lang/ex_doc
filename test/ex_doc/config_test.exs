defmodule ExDoc.ConfigTest do
  use ExUnit.Case, async: true

  @project "Elixir"
  @version "1"

  test "normalizes output" do
    opts_with_output = &[source_beam: "beam_dir", output: &1]

    config = ExDoc.Config.build(@project, @version, opts_with_output.("test/tmp/ex_doc"))
    assert config.output == "test/tmp/ex_doc"

    config = ExDoc.Config.build(@project, @version, opts_with_output.("test/tmp/ex_doc/"))
    assert config.output == "test/tmp/ex_doc"

    config = ExDoc.Config.build(@project, @version, opts_with_output.("test/tmp/ex_doc//"))
    assert config.output == "test/tmp/ex_doc"
  end

  test "normalizes filter_modules" do
    config = ExDoc.Config.build(@project, @version, filter_modules: "^Elixir\\.Foo")
    assert config.filter_modules.(Foo, %{})
    refute config.filter_modules.(Bar, %{})

    config = ExDoc.Config.build(@project, @version, filter_modules: ~r/^Elixir.Foo/)
    assert config.filter_modules.(Foo, %{})
    refute config.filter_modules.(Bar, %{})

    config = ExDoc.Config.build(@project, @version, filter_modules: fn _mod, m -> m[:works] end)
    assert config.filter_modules.(Foo, %{works: true})
    refute config.filter_modules.(Foo, %{works: false})
  end

  test "normalizes skip_undefined_reference_warnings_on" do
    config =
      ExDoc.Config.build(@project, @version,
        skip_undefined_reference_warnings_on: ["Foo", "Bar.baz/0"]
      )

    assert config.skip_undefined_reference_warnings_on.("Foo")
    assert config.skip_undefined_reference_warnings_on.("Bar.baz/0")
    refute config.skip_undefined_reference_warnings_on.("Foo.bar/1")

    config =
      ExDoc.Config.build(@project, @version,
        skip_undefined_reference_warnings_on: &String.match?(&1, ~r/Foo/)
      )

    assert config.skip_undefined_reference_warnings_on.("Foo")
    refute config.skip_undefined_reference_warnings_on.("Bar.baz/0")
    assert config.skip_undefined_reference_warnings_on.("Foo.bar/1")
  end

  test "normalizes skip_code_autolink_to" do
    config =
      ExDoc.Config.build(@project, @version,
        skip_code_autolink_to: ["ConfigTest.Hidden", "ConfigTest.Hidden.foo/1"]
      )

    assert config.skip_code_autolink_to.("ConfigTest.Hidden")
    assert config.skip_code_autolink_to.("ConfigTest.Hidden.foo/1")
    refute config.skip_code_autolink_to.("ConfigTest.Hidden.foo/2")
    refute config.skip_code_autolink_to.("ConfigTest.Hidden.bar/1")
    refute config.skip_code_autolink_to.("ConfigTest.NotHidden")

    config =
      ExDoc.Config.build(@project, @version,
        skip_code_autolink_to: &String.match?(&1, ~r/\AConfigTest\.Hidden/)
      )

    assert config.skip_code_autolink_to.("ConfigTest.Hidden")
    assert config.skip_code_autolink_to.("ConfigTest.Hidden.foo/1")
    assert config.skip_code_autolink_to.("ConfigTest.Hidden.foo/2")
    assert config.skip_code_autolink_to.("ConfigTest.Hidden.bar/1")
    refute config.skip_code_autolink_to.("ConfigTest.NotHidden")
  end
end
