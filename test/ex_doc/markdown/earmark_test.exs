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
             end) =~ "Closing unclosed backquotes ` at end of input"
    end

    test "handles warnings" do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert [{:p, [], _, %{}}] =
                        Markdown.to_ast("{:ok, status, %MyApp.User{}} on success", [])
             end) =~ "ignored in IAL"
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

    test "leaves blockquotes with the wrong markup as is" do
      not_admonition = """
      > ### H3 {: .xyz}
      > This is NOT an admonition!
      """

      assert Markdown.to_ast(not_admonition, []) == [
               {:blockquote, [],
                [
                  {:h3, [class: "xyz"], ["H3"], %{}},
                  {:p, [], ["This is NOT an admonition!"], %{}}
                ], %{}}
             ]

      no_h_tag_beginning = """
      > {: .warning}
      > #### Warning {: .warning}
      > This blockquote didn't start with the h4 tag, so it wasn't converted.
      """

      assert Markdown.to_ast(no_h_tag_beginning, []) == [
               {:blockquote, [],
                [
                  {:p, [], ["{: .warning}"], %{}},
                  {:h4, [class: "warning"], ["Warning"], %{}},
                  {:p, [],
                   ["This blockquote didn't start with the h4 tag, so it wasn't converted."], %{}}
                ], %{}}
             ]
    end

    test "converts blockquotes with the appropriate markup to admonition sections" do
      info = """
      > #### Info {: .info .ignore}
      > This is info.
      """

      assert [
               {:section, section_attrs,
                [
                  {:h4, h_attrs, ["Info"], %{}},
                  {:p, [], ["This is info."], %{}}
                ], %{}}
             ] = Markdown.to_ast(info, [])

      assert section_attrs[:role] == "note"
      assert section_attrs[:class] == "admonition info"

      assert h_attrs[:class] == "admonition-title ignore info"

      warning_error = """
      > ### Warning! Error! {: .warning .error}
      > A warning and an error.
      """

      assert [
               {:section, section_attrs,
                [
                  {:h3, h_attrs, ["Warning! Error!"], %{}},
                  {:p, [], ["A warning and an error."], %{}}
                ], %{}}
             ] = Markdown.to_ast(warning_error, [])

      assert section_attrs[:role] == "note"
      assert section_attrs[:class] == "admonition error warning"

      assert h_attrs[:class] == "admonition-title error warning"

      with_blockquote_level_attrs = """
      > ### Eggs and baskets  {: .tip}
      > Don't put all your eggs in one basket, especially if they're golden.
      {: .egg-basket-bg #egg-basket-tip}
      """

      assert [
               {:section, section_attrs,
                [
                  {:h3, h_attrs, ["Eggs and baskets"], %{}},
                  {:p, [],
                   ["Don't put all your eggs in one basket, especially if they're golden."], %{}}
                ], %{}}
             ] = Markdown.to_ast(with_blockquote_level_attrs, [])

      assert section_attrs[:role] == "note"
      assert section_attrs[:class] == "admonition tip egg-basket-bg"
      assert section_attrs[:id] == "egg-basket-tip"

      assert h_attrs[:class] == "admonition-title tip"
    end

    test "keeps math syntax without interpreting math as markdown" do
      assert Markdown.to_ast("Math $x *y* y$", []) == [
               {:p, [], ["Math ", "$x *y* y$"], %{}}
             ]

      assert Markdown.to_ast("Math $$x$$", []) == [
               {:p, [], ["Math ", "$$\nx\n$$"], %{}}
             ]
    end
  end
end
