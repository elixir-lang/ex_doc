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

  test "produces a function when a debug_info_key is provided" do
    config = ExDoc.Config.build(@project, @version, debug_info_key: "Hunter2")

    assert config.debug_info_fn.(:init) == :ok
    assert config.debug_info_fn.(:clear) == :ok
    assert config.debug_info_fn.({:debug_info, nil, nil, nil}) == ~c"Hunter2"

    config = ExDoc.Config.build(@project, @version, debug_info_key: {:des3_cbc, "Hunter3"})

    assert config.debug_info_fn.(:init) == :ok
    assert config.debug_info_fn.(:clear) == :ok
    assert config.debug_info_fn.({:debug_info, nil, nil, nil}) == ~c"Hunter3"
  end

  test "ignores debug_info_key when debug_info_fn or debug_info_fun is provided" do
    config =
      ExDoc.Config.build(@project, @version,
        debug_info_key: "Hunter2",
        debug_info_fn: debug_info_fn(~c"foxtrot")
      )

    assert config.debug_info_fn.({:debug_info, nil, nil, nil}) == ~c"foxtrot"

    config =
      ExDoc.Config.build(@project, @version,
        debug_info_key: "Hunter2",
        debug_info_fun: debug_info_fn(~c"tango")
      )

    assert config.debug_info_fn.({:debug_info, nil, nil, nil}) == ~c"tango"
  end

  test "handles either debug_info_fn or debug_info_fun, but debug_info_fn takes precedence" do
    config =
      ExDoc.Config.build(@project, @version,
        debug_info_fun: debug_info_fn(~c"fun"),
        debug_info_fn: debug_info_fn(~c"fn")
      )

    assert config.debug_info_fn.({:debug_info, nil, nil, nil}) == ~c"fn"

    config = ExDoc.Config.build(@project, @version, debug_info_fun: debug_info_fn(~c"fun"))

    assert config.debug_info_fn.({:debug_info, nil, nil, nil}) == ~c"fun"
  end

  defp debug_info_fn(key) do
    fn
      :init -> :ok
      :clear -> :ok
      {:debug_info, _mode, _module, _filename} -> key
    end
  end
end
