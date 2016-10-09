defmodule ExDoc.Markdown.PandocTest do
  use ExUnit.Case, async: true

  alias ExDoc.Markdown.Pandoc, as: Markdown

  @moduletag :pandoc

  test "to_html generate the HTML from the markdown" do
    assert Markdown.to_html("# Test", []) == ~s(<h1 id="test">Test</h1>\n)
    assert Markdown.to_html("# Test Other", header_level: 2) == ~s(<h2 id="test-other">Test Other</h2>\n)
    assert Markdown.to_html("# Test Another", header_level: 6) == ~s(<h6 id="test-another">Test Another</h6>\n)
  end

  test "to_html raises when the value passed is not a binary" do
    assert_raise FunctionClauseError, fn ->
      Markdown.to_html(nil, [])
    end
  end

  test "to_html autolink http address" do
    result = Markdown.to_html("<https://github.com/elixir-lang>", [])
    assert result =~ ~r(href="https://github.com/elixir-lang")
    assert result =~ ~r(>https://github.com/elixir-lang<)
  end

  test "to_html handles empty input" do
    assert Markdown.to_html("", []) == "\n"
  end

  test "to_html converts to rst" do
    assert Markdown.to_html("`hello`", format: "rst") == "``hello``\n"
  end

  test "pretty Markdown fenced code blocks for Pandoc" do
    pandoc_with_language_specified = "```elixir\nmix run --no-halt path/to/file.exs\n```"
    expected = "<pre><code class=\"elixir\">mix run --no-halt path/to/file.exs</code></pre>\n"
    assert Markdown.to_html(pandoc_with_language_specified, []) == expected
  end
end
