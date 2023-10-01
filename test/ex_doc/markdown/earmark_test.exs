defmodule ExDoc.Markdown.EarmarkTest do
  use ExUnit.Case

  alias ExDoc.Markdown.Earmark, as: Markdown

  @moduletag :earmark

  describe "to_ast/2" do
    test "generate AST" do
      assert Markdown.to_ast("# Test\n\nHello", []) == [
               {:h1, [], ["Test"], %{}},
               {:p, [], ["Hello"], %{}}
             ]

      assert Markdown.to_ast("[foo](bar)", []) == [
               {:p, [], [{:a, [href: "bar"], ["foo"], %{}}], %{}}
             ]

      assert Markdown.to_ast("<p><em>Test</em></p>", []) == [
               {:p, [], ["<em>Test</em>"], %{verbatim: true}}
             ]
    end

    test "empty input" do
      assert Markdown.to_ast("", []) == []
    end

    test "comments" do
      assert Markdown.to_ast("<!-- INCLUDE -->", []) ==
               [{:comment, [], [" INCLUDE "], %{comment: true}}]
    end

    test "warnings" do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               Markdown.to_ast("`foo", [])
             end) =~ "(warning) nofile:1 Closing unclosed backquotes ` at end of input"
    end

    test "handles warnings" do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert [{:p, [], _, %{}}] =
                        Markdown.to_ast("{:ok, status, %MyApp.User{}} on success", [])
             end) =~ "ExDoc.Markdown.Earmark (warning)"
    end

    test "rewrites livebook outputs to output code blocks" do
      md = """
      # Notebook

      ## Example

      ```elixir
      1 + 1
      ```

      <!-- livebook:{"output":true} -->

      ```
      2
      ```

      <!-- livebook:{"output":true} -->

      ```mermaid
      graph TD; A-->B;
      ```
      """

      assert Markdown.to_ast(md, []) == [
               {:h1, [], ["Notebook"], %{}},
               {:h2, [], ["Example"], %{}},
               {:pre, [], [{:code, [class: "elixir"], ["1 + 1"], %{}}], %{}},
               {:pre, [], [{:code, [class: "output"], ["2"], %{}}], %{}},
               {:pre, [], [{:code, [class: "mermaid output"], ["graph TD; A-->B;"], %{}}], %{}}
             ]
    end
  end
end
