defmodule ExDoc.Markdown.EarmarkTest do
  use ExUnit.Case, async: true

  alias ExDoc.Markdown.Earmark, as: Markdown

  @moduletag :earmark

  describe "to_ast/2" do
    test "generate AST" do
      assert Markdown.to_ast("# Test\n\nHello", []) == [{:h1, [], ["Test"]}, {:p, [], ["Hello"]}]
      assert Markdown.to_ast("[foo](bar)", []) == [{:p, [], [{:a, [href: "bar"], ["foo"]}]}]
      assert Markdown.to_ast("<p>\nTest\n</p>", []) == [{:p, '', ["Test"]}]
    end

    test "empty input" do
      assert Markdown.to_ast("", []) == []
    end

    test "comments" do
      assert Markdown.to_ast("<!-- INCLUDE -->", []) == []
    end

    test "warnings" do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               Markdown.to_ast("`foo", [])
             end) =~ "(warning) nofile:1 Closing unclosed backquotes ` at end of input"
    end

    test "bug #1222" do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert [{:p, [], _}] =
                        Markdown.to_ast("{:ok, status, %MyApp.User{}} on success", [])
             end) =~ "ExDoc.Markdown.Earmark (warning)"
    end
  end
end
