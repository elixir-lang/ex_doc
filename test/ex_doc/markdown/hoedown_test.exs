defmodule ExDoc.Markdown.HoedownTest do
  use ExUnit.Case, async: true

  @moduletag :hoedown

  alias ExDoc.Markdown.Hoedown, as: Markdown

  test "to_html generate the HTML from the markdown" do
    assert Markdown.to_html("# Test") == "<h1>Test</h1>\n"
  end

  test "to_html raises an ArgumentError if the value passed is nil" do
    assert_raise ArgumentError, fn ->
      Markdown.to_html(nil)
    end
  end

  test "to_html autolink http address" do
    expected = ~s(<p><a href="https://github.com/elixir-lang">https://github.com/elixir-lang</a></p>\n)
    assert Markdown.to_html("https://github.com/elixir-lang") == expected
  end

  test "to_html handles empty input" do
    assert Markdown.to_html("") == ""
  end
end
