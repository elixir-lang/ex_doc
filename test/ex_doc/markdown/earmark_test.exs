defmodule ExDoc.Markdown.EarmarkTest do
  use ExUnit.Case, async: true

  alias ExDoc.Markdown.Earmark, as: Markdown

  @moduletag :earmark

  describe "to_ast/1" do
    test "generate AST" do
      assert Markdown.to_ast("# Test\n\nHello", []) == [{:h1, [], ["Test"]}, {:p, [], ["Hello"]}]
      assert Markdown.to_ast("[foo](bar)", []) == [{:p, [], [{:a, [href: "bar"], ["foo"]}]}]
    end

    test "empty input" do
      assert Markdown.to_ast("", []) == []
    end
  end
end
