defmodule ExDoc.Retriever.SourcePatternTest do
  use ExUnit.Case, async: true
  alias ExDoc.Retriever
  import TestHelper

  @moduletag :tmp_dir

  def source_url_fn_1(path, line),
    do: "%{path}:%{line}" |> String.replace("%{path}", path) |> String.replace("%{line}", line)

  def source_url_fn_2(path, line),
    do: "%{path}-%{line}" |> String.replace("%{path}", path) |> String.replace("%{line}", line)

  describe "docs_from_modules/2" do
    test "callbacks with source_url_pattern as function", c do
      elixirc(c, ~S"""
      defmodule ModSource do
        @doc "Sample docs."
        @callback callback1() :: :ok
      end
      """)

      config = %ExDoc.Config{source_url_pattern: &source_url_fn_1/2}
      [mod] = Retriever.docs_from_modules([ModSource], config)
      [callback1] = mod.docs
      assert Path.basename(callback1.source_url) == "nofile:3"

      config = %ExDoc.Config{source_url_pattern: &source_url_fn_2/2}
      [mod] = Retriever.docs_from_modules([ModSource], config)
      [callback1] = mod.docs
      assert Path.basename(callback1.source_url) == "nofile-3"
    end
  end
end
