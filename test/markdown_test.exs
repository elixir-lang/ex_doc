Code.require_file "../test_helper.exs", __FILE__

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

  test "to_html autolink http address" do
    expected = %b(<p><a href="https://github.com/elixir-lang">https://github.com/elixir-lang</a></p>\n)
    assert Markdown.to_html("https://github.com/elixir-lang") == expected
  end

  test "to_html handles empty input" do
    assert Markdown.to_html("") == ""
  end

  test "autolink fun/arity in module" do
    assert Markdown.autolink_locals("`example/2`", ["example/2"]) == "[`example/2`](#example/2)"
    assert Markdown.autolink_locals("`example/2` then `example/2`", 
      ["example/2"]) == "[`example/2`](#example/2) then [`example/2`](#example/2)"
    assert Markdown.autolink_locals("`  spaces/0  `", ["spaces/0"]) ==
      "[`  spaces/0  `](#spaces/0)"
    assert Markdown.autolink_locals("`example/1` and `example/2`", 
      ["example/1", "example/2"]) == "[`example/1`](#example/1) and [`example/2`](#example/2)"
    assert Markdown.autolink_locals("`funny_name\?/1` and `funny_name!/2`", 
      ["funny_name\?/1", "funny_name!/2"]) == 
      "[`funny_name\?/1`](#funny_name\?/1) and [`funny_name!/2`](#funny_name!/2)"
  end

  test "autolink doesn't create links for undefined functions" do
    assert Markdown.autolink_locals("`example/1`", ["example/2"]) == "`example/1`"
    assert Markdown.autolink_locals("`example/1`", []) == "`example/1`"
  end

  test "autolink doesn't create links for pre-linked functions" do
    assert Markdown.autolink_locals("[`example/1`]()", ["example/1"]) == "[`example/1`]()"
    assert Markdown.autolink_locals("[the `example/1`]()", ["example/1"]) == "[the `example/1`]()"
  end
end
