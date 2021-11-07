defmodule ExDoc.ConfigTest do
  use ExUnit.Case, async: true

  @project "Elixir"
  @version "1"

  test "normalizes output" do
    opts_with_output =
      &[source_beam: "beam_dir", output: &1]

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
end
