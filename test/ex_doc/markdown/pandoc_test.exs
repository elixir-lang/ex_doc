defmodule MarkdownTest.PandocTest do
  use ExUnit.Case, async: true

  alias ExDoc.Markdown.Pandoc, as: Markdown

  @moduletag :pandoc

  test "to_html generate the HTML from the markdown" do
    assert Markdown.to_html("# Test") == ~s(<h1 id="test">Test</h1>\n)
    assert Markdown.to_html("# Test Other", 2) == ~s(<h2 id="test-other">Test Other</h2>\n)
    assert Markdown.to_html("# Test Another", 6) == ~s(<h6 id="test-another">Test Another</h6>\n)
  end

  test "to_html raises an ArgumentError if the value passed is nil" do
    assert_raise ArgumentError, fn ->
      Markdown.to_html(nil)
    end
  end

  test "to_html autolink http address" do
    expected = ~s(<p><a href="https://github.com/elixir-lang">https://github.com/elixir-lang</a></p>\n)
    assert Markdown.to_html("<https://github.com/elixir-lang>") == expected
  end

  test "to_html handles empty input" do
    assert Markdown.to_html("") == "\n"
  end
end
