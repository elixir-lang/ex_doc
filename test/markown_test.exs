Code.require_file "../test_helper", __FILE__

defmodule MarkdownTest do
  use ExUnit.Case

  test "to_html generate the HTML from the markdown" do
    assert Markdown.to_html("# Test") == "<h1>Test</h1>\n"
  end

  test "to_html raises an ArgumentError if the value passed is nil" do
    assert_raise ArgumentError, fn ->
      Markdown.to_html(nil)
    end
  end
end
