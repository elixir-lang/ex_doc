defmodule ExDoc.Markdown.CmarkTest do
  use ExUnit.Case, async: true

  @moduletag :cmark

  alias ExDoc.Markdown.Cmark, as: Markdown

  test "to_html generate the HTML from the markdown" do
    assert Markdown.to_html("# Test\n\nHello", []) =~
          ~s(<h1>Test</h1>\n<p>Hello</p>)
  end

  test "to_html handles empty input" do
    assert Markdown.to_html("", []) == ""
  end
end
