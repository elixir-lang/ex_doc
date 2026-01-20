defmodule ExDoc.DocASTTest do
  use ExUnit.Case, async: true
  alias ExDoc.DocAST

  defp parse!(text, format, opts \\ []) do
    opts = [markdown_processor: ExDoc.Markdown.Earmark] ++ opts
    DocAST.parse!(text, format, opts)
  end

  describe "parse!/3" do
    test "markdown" do
      markdown = """
      **foo**
      """

      assert parse!(markdown, "text/markdown") ==
               [{:p, [], [{:strong, [], ["foo"], %{}}], %{}}]
    end

    test "markdown comments" do
      markdown = """
      **foo**
      <!-- bar -->
      """

      assert parse!(markdown, "text/markdown") ==
               [
                 {:p, [], [{:strong, [], ["foo"], %{}}], %{}},
                 {:comment, [], [" bar "], %{comment: true}}
               ]
    end

    test "markdown errors" do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert parse!("`String.upcase", "text/markdown") ==
                        [{:p, [], ["`String.upcase"], %{}}]
             end) =~ "Closing unclosed backquotes ` at end of input"
    end

    test "erlang+html" do
      erl_ast = [{:p, [], ["foo"]}]

      assert parse!(erl_ast, "application/erlang+html") ==
               [{:p, [], ["foo"], %{}}]
    end

    test "erlang+html code blocks" do
      erl_ast = [{:pre, [], ["foo"]}]

      assert parse!(erl_ast, "application/erlang+html") ==
               [{:pre, [], [{:code, [], ["foo"], %{}}], %{}}]
    end
  end

  describe "to_string/2" do
    test "simple" do
      markdown = """
      foo **bar** baz
      """

      ast = parse!(markdown, "text/markdown")

      assert DocAST.to_html(ast) ==
               ~s{<p>foo <strong>bar</strong> baz</p>}
    end

    test "comments" do
      markdown = """
      hello
      <!-- HTML -->
      """

      ast = parse!(markdown, "text/markdown")

      assert DocAST.to_html(ast) ==
               ~s{<p>hello</p><!-- HTML -->}
    end

    test "escape" do
      markdown = "foo > bar"
      ast = parse!(markdown, "text/markdown")
      assert DocAST.to_html(ast) == ~s{<p>foo &gt; bar</p>}
    end

    test "raw html" do
      markdown = "<strong><em>bar</em></strong>"
      ast = parse!(markdown, "text/markdown")
      assert DocAST.to_html(ast) == ~s{<strong><em>bar</em></strong>}
    end

    test "empty" do
      markdown = """
      <span>
      <i></i>
      <i></i>
      </span>
      """

      ast = parse!(markdown, "text/markdown")
      assert DocAST.to_html(ast) == ~s{<span><i></i>\n<i></i></span>}
    end

    test "void elements" do
      markdown = """
      foo\s\s
      bar
      """

      ast = parse!(markdown, "text/markdown")

      assert DocAST.to_html(ast) == ~s{<p>foo<br/>bar</p>}
    end
  end

  describe "synopsis" do
    test "functionality" do
      assert synopsis("") == ""
      assert synopsis(".") == "<p>.</p>"
      assert synopsis("::") == "<p></p>"
      assert synopsis("abcd") == "<p>abcd</p>"
      assert synopsis("Description:") == "<p>Description</p>"

      assert synopsis("[Access](Access.html)") ==
               "<p><a href=\"Access.html\">Access</a></p>"

      assert synopsis("[Access](Access.html) {: #foo}") ==
               "<p><a href=\"Access.html\">Access</a></p>"
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
      |> parse!("text/markdown")
      |> DocAST.synopsis()
    end
  end

  describe "headers" do
    test "adds and extracts anchored headers" do
      assert """
             # h1

             ## h2

             ### h3 repeat

             ## h2 > h3

             ### h3 repeat

             > ## inside `blockquote`
             """
             |> parse!("text/markdown")
             |> DocAST.add_ids_to_headers([:h2, :h3])
             |> DocAST.extract_headers_with_ids([:h2, :h3]) ==
               [
                 {:h2, "h2", "h2"},
                 {:h3, "h3 repeat", "h3-repeat"},
                 {:h2, "h2 > h3", "h2-h3"},
                 {:h3, "h3 repeat", "h3-repeat-1"},
                 {:h2, "inside blockquote", "inside-blockquote"}
               ]
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

      # Nested in another element
      assert highlight("""
             > ```elixir
             > hello
             > ```
             """) =~
               ~r{<blockquote><pre><code class=\"makeup elixir\" translate="no">.*}
    end

    defp highlight(markdown) do
      markdown
      |> parse!("text/markdown")
      |> DocAST.highlight(ExDoc.Language.Elixir)
      |> DocAST.to_html()
    end
  end

  describe "sectionize" do
    test "sectioninize" do
      assert """
             # H1

             ## H2-1 {:class="example"}

             p1

             ### H3-1

             p2

             ### H3-2

             p3

             ### H3-3

             p4

             ## H2-2

             p5

             ### H3-1 {:class="last"}

             p6
             """
             |> parse!("text/markdown")
             |> DocAST.sectionize([:h2, :h3]) ==
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
