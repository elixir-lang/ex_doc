defmodule ExDoc.DocASTTest do
  use ExUnit.Case, async: true
  alias ExDoc.DocAST

  describe "parse!/3" do
    test "markdown" do
      markdown = """
      **foo**
      """

      assert DocAST.parse!(markdown, "text/markdown") ==
               [{:p, [], [{:strong, [], ["foo"], %{}}], %{}}]
    end

    test "markdown comments" do
      markdown = """
      **foo**
      <!-- bar -->
      """

      assert DocAST.parse!(markdown, "text/markdown") ==
               [
                 {:p, [], [{:strong, [], ["foo"], %{}}], %{}},
                 {:comment, [], [" bar "], %{comment: true}}
               ]
    end

    test "markdown errors" do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert DocAST.parse!("`String.upcase", "text/markdown") ==
                        [{:p, [], ["`String.upcase"], %{}}]
             end) =~ "Closing unclosed backquotes ` at end of input"
    end

    test "erlang+html" do
      erl_ast = [{:p, [], ["foo"]}]

      assert DocAST.parse!(erl_ast, "application/erlang+html") ==
               [{:p, [], ["foo"], %{}}]
    end

    test "erlang+html code blocks" do
      erl_ast = [{:pre, [], ["foo"]}]

      assert DocAST.parse!(erl_ast, "application/erlang+html") ==
               [{:pre, [], [{:code, [], ["foo"], %{}}], %{}}]
    end
  end

  describe "to_string/2" do
    test "simple" do
      markdown = """
      foo **bar** baz
      """

      ast = DocAST.parse!(markdown, "text/markdown")

      assert DocAST.to_string(ast) ==
               ~s{<p>foo <strong>bar</strong> baz</p>}
    end

    test "escape" do
      markdown = "foo > bar"
      ast = DocAST.parse!(markdown, "text/markdown")
      assert DocAST.to_string(ast) == ~s{<p>foo &gt; bar</p>}
    end

    test "raw html" do
      markdown = "<strong><em>bar</em></strong>"
      ast = DocAST.parse!(markdown, "text/markdown")
      assert DocAST.to_string(ast) == ~s{<strong><em>bar</em></strong>}
    end

    test "empty" do
      markdown = """
      <span>
      <i></i>
      <i></i>
      </span>
      """

      ast = DocAST.parse!(markdown, "text/markdown")
      assert DocAST.to_string(ast) == ~s{<span><i></i>\n<i></i></span>}
    end

    test "void elements" do
      markdown = """
      foo\s\s
      bar
      """

      ast = DocAST.parse!(markdown, "text/markdown")

      assert DocAST.to_string(ast) == ~s{<p>foo<br/>bar</p>}
    end
  end

  describe "synopsis" do
    test "functionality" do
      assert synopsis("") == ""
      assert synopsis("<p>.</p>") == "<p>.</p>"
      assert synopsis("<p>::</p>") == "<p></p>"
      assert synopsis("<p>Description:</p>") == "<p>Description</p>"
      assert synopsis("<p>abcd</p>") == "<p>abcd</p>"
    end

    test "should not end have trailing periods or semicolons" do
      doc1 = """
      Summaries should not be displayed with trailing semicolons :

      ## Example
      """

      doc2 = """
      Example function: Summary should display trailing period :.

      ## Example:
      """

      assert synopsis(doc1) ==
               "<p>Summaries should not be displayed with trailing semicolons </p>"

      assert synopsis(doc2) ==
               "<p>Example function: Summary should display trailing period :.</p>"
    end

    defp synopsis(markdown) do
      markdown
      |> ExDoc.DocAST.parse!("text/markdown")
      |> ExDoc.DocAST.synopsis()
    end
  end

  describe "extract_headers" do
    test "extracts h2 headers" do
      assert extract_headers("""
             # h1
             ## h2-a
             ### h3
             ## h2-b
             ##
             """) == ["h2-a", "h2-b"]
    end

    test "trims whitespace and preserve HTML entities" do
      assert extract_headers("""
             # h1
             ##\s\s\s**h2**\s<&>\s`h2`\s\s\s
             """) == ["h2 <&> h2"]
    end

    defp extract_headers(markdown) do
      markdown
      |> ExDoc.DocAST.parse!("text/markdown")
      |> ExDoc.DocAST.extract_headers([:h2])
    end
  end

  describe "highlight" do
    test "with default class" do
      # Four spaces
      assert highlight("""
                 mix run --no-halt path/to/file.exs
             """) =~
               ~r{<pre><code class=\"makeup elixir\" translate="no">.*}

      # Code block without language
      assert highlight("""
             ```
             mix run --no-halt path/to/file.exs</code></pre>
             ```
             """) =~
               ~r{<pre><code class=\"makeup elixir\" translate="no">.*}

      # Pre IAL
      assert highlight("""
             ```
             mix run --no-halt path/to/file.exs</code></pre>
             ```
             {:class="wrap"}
             """) =~
               ~r{<pre class="wrap"><code class=\"makeup elixir\" translate="no">.*}

      # Code with language
      assert highlight("""
             ```html
             <foo />
             ```
             """) =~
               ~r{<pre><code class=\"makeup html\" translate="no">.*}

      # Code with shell detection
      assert highlight("""
             ```
             $ hello
             ```
             """) =~
               ~r{<pre><code class=\"makeup shell\" translate="no"><span class="gp unselectable">\$.*}
    end

    defp highlight(markdown) do
      markdown
      |> ExDoc.DocAST.parse!("text/markdown")
      |> ExDoc.DocAST.highlight(ExDoc.Language.Elixir)
      |> ExDoc.DocAST.to_string()
    end
  end

  describe "sectionize" do
    test "sectioninize" do
      list = [
        {:h1, [], ["H1"], %{}},
        {:h2, [class: "example"], ["H2-1"], %{}},
        {:p, [], ["p1"], %{}},
        {:h3, [], ["H3-1"], %{}},
        {:p, [], ["p2"], %{}},
        {:h3, [], ["H3-2"], %{}},
        {:p, [], ["p3"], %{}},
        {:h3, [], ["H3-3"], %{}},
        {:p, [], ["p4"], %{}},
        {:h2, [], ["H2-2"], %{}},
        {:p, [], ["p5"], %{}},
        {:h3, [class: "last"], ["H3-1"], %{}},
        {:p, [], ["p6"], %{}}
      ]

      assert DocAST.sectionize(list, [:h2, :h3]) ==
               [
                 {:h1, [], ["H1"], %{}},
                 {:section, [class: "h2 example"],
                  [
                    {:h2, [class: "example"], ["H2-1"], %{}},
                    {:p, [], ["p1"], %{}},
                    {:section, [class: "h3"],
                     [
                       {:h3, [], ["H3-1"], %{}},
                       {:p, [], ["p2"], %{}}
                     ], %{}},
                    {:section, [class: "h3"],
                     [
                       {:h3, [], ["H3-2"], %{}},
                       {:p, [], ["p3"], %{}}
                     ], %{}},
                    {:section, [class: "h3"],
                     [
                       {:h3, [], ["H3-3"], %{}},
                       {:p, [], ["p4"], %{}}
                     ], %{}}
                  ], %{}},
                 {:section, [class: "h2"],
                  [
                    {:h2, [], ["H2-2"], %{}},
                    {:p, [], ["p5"], %{}},
                    {:section, [class: "h3 last"],
                     [
                       {:h3, [class: "last"], ["H3-1"], %{}},
                       {:p, [], ["p6"], %{}}
                     ], %{}}
                  ], %{}}
               ]
    end
  end
end
