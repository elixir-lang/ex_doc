defmodule ExDoc.Markdown.EarmarkTest do
  use ExUnit.Case, async: true

  alias ExDoc.Markdown.Earmark, as: Markdown

  @moduletag :earmark

  test "to_html generate the HTML from the markdown" do
    assert Markdown.to_html("# Test\n\nHello", []) =~
          ~s(<h1>Test</h1>\n<p>Hello</p>)
  end

  test "to_html handles empty input" do
    assert Markdown.to_html("", []) == ""
  end
end
