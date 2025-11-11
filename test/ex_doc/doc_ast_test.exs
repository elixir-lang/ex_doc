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

  describe "to_markdown/1" do
    test "converts simple text" do
      assert DocAST.to_markdown("hello world") == "hello world"
    end

    test "escapes HTML entities in text" do
      assert DocAST.to_markdown("<script>alert('xss')</script>") ==
               "&lt;script&gt;alert('xss')&lt;/script&gt;"

      assert DocAST.to_markdown("Tom & Jerry") == "Tom &amp; Jerry"
    end

    test "converts lists of elements" do
      ast = ["Hello ", "world", "!"]
      assert DocAST.to_markdown(ast) == "Hello world!"
    end

    test "converts paragraphs" do
      ast = {:p, [], ["Hello world"], %{}}
      assert DocAST.to_markdown(ast) == "Hello world\n\n"
    end

    test "converts multiple paragraphs" do
      ast = [
        {:p, [], ["First paragraph"], %{}},
        {:p, [], ["Second paragraph"], %{}}
      ]

      assert DocAST.to_markdown(ast) == "First paragraph\n\nSecond paragraph\n\n"
    end

    test "converts code blocks with language" do
      ast = {:code, [class: "elixir"], "defmodule Test do\n  def hello, do: :world\nend", %{}}
      expected = "```elixir\ndefmodule Test do\n  def hello, do: :world\nend\n```\n"
      assert DocAST.to_markdown(ast) == expected
    end

    test "converts code blocks without language" do
      ast = {:code, [], "some code", %{}}
      assert DocAST.to_markdown(ast) == "```\nsome code\n```\n"
    end

    test "converts inline code with class attribute" do
      ast = {:code, [class: "language-elixir"], "IO.puts", %{}}
      expected = "```language-elixir\nIO.puts\n```\n"
      assert DocAST.to_markdown(ast) == expected
    end

    test "converts links" do
      ast = {:a, [href: "https://example.com"], ["Example"], %{}}
      assert DocAST.to_markdown(ast) == "[Example](https://example.com)"
    end

    test "converts links with nested content" do
      ast = {:a, [href: "/docs"], [{:code, [], ["API"], %{}}], %{}}
      assert DocAST.to_markdown(ast) == "[```\nAPI\n```\n](/docs)"
    end

    test "converts images with alt and title" do
      ast = {:img, [src: "image.png", alt: "Alt text", title: "Title"], [], %{}}
      assert DocAST.to_markdown(ast) == "![Alt text](image.png \"Title\")"
    end

    test "converts images with missing attributes" do
      ast = {:img, [src: "image.png"], [], %{}}
      assert DocAST.to_markdown(ast) == "![](image.png \"\")"
    end

    test "converts horizontal rules" do
      ast = {:hr, [], [], %{}}
      assert DocAST.to_markdown(ast) == "\n\n---\n\n"
    end

    test "converts line breaks" do
      ast = {:br, [], [], %{}}
      assert DocAST.to_markdown(ast) == "\n\n"
    end

    test "converts comments" do
      ast = {:comment, [], [" This is a comment "], %{}}
      assert DocAST.to_markdown(ast) == "<!-- This is a comment -->"
    end

    test "handles void elements" do
      void_elements = [
        :area,
        :base,
        :col,
        :embed,
        :input,
        :link,
        :meta,
        :param,
        :source,
        :track,
        :wbr
      ]

      for element <- void_elements do
        ast = {element, [], [], %{}}
        assert DocAST.to_markdown(ast) == ""
      end
    end

    test "handles verbatim content" do
      ast = {:pre, [], ["  verbatim  \n  content  "], %{verbatim: true}}
      assert DocAST.to_markdown(ast) == "  verbatim  \n  content  "
    end

    test "converts nested structures" do
      ast = {:p, [], ["Hello ", {:strong, [], ["world"], %{}}, "!"], %{}}

      result = DocAST.to_markdown(ast)
      assert result =~ "Hello"
      assert result =~ "world"
      assert result =~ "!"
      assert String.ends_with?(result, "\n\n")
    end

    test "handles unknown elements by extracting content" do
      ast = {:custom_element, [class: "special"], ["Content"], %{}}
      assert DocAST.to_markdown(ast) == "Content"
    end

    test "handles complex nested document" do
      ast = [
        {:h1, [], ["Main Title"], %{}},
        {:p, [], ["Introduction paragraph with ", {:a, [href: "/link"], ["a link"], %{}}], %{}},
        {:code, [class: "elixir"], "IO.puts \"Hello\"", %{}},
        {:hr, [], [], %{}},
        {:p, [], ["Final paragraph"], %{}}
      ]

      result = DocAST.to_markdown(ast)

      assert result =~ "Main Title"
      assert result =~ "Introduction paragraph with [a link](/link)"
      assert result =~ "```elixir\nIO.puts \"Hello\"\n```\n"
      assert result =~ "\n\n---\n\n"
      assert result =~ "Final paragraph\n\n"
    end

    test "handles empty content gracefully" do
      assert DocAST.to_markdown([]) == ""
      assert DocAST.to_markdown({:p, [], [], %{}}) == "\n\n"
    end

    test "preserves whitespace in code blocks" do
      code_content = "  def hello do\n    :world\n  end"
      ast = {:code, [class: "elixir"], code_content, %{}}
      result = DocAST.to_markdown(ast)

      assert result =~ "```elixir"
      assert String.contains?(result, code_content)
      assert result =~ "```"
    end

    test "handles mixed content types" do
      ast = [
        "Plain text",
        {:p, [], ["Paragraph text"], %{}},
        {:code, [], "code", %{}},
        "More plain text"
      ]

      result = DocAST.to_markdown(ast)
      assert result =~ "Plain text"
      assert result =~ "Paragraph text\n\n"
      assert result =~ "```\ncode\n```\n"
      assert result =~ "More plain text"
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
      |> DocAST.parse!("text/markdown")
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
             |> DocAST.parse!("text/markdown")
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
      assert highlight("""
                 mix run --no-halt path/to/file.exs
             """) =~
               ~r{<pre><code class=\"makeup elixir\" translate="no">.*}

      assert highlight("""
             ```
             mix run --no-halt path/to/file.exs</code></pre>
             ```
             """) =~
               ~r{<pre><code class=\"makeup elixir\" translate="no">.*}

      assert highlight("""
             ```
             mix run --no-halt path/to/file.exs</code></pre>
             ```
             {:class="wrap"}
             """) =~
               ~r{<pre class="wrap"><code class=\"makeup elixir\" translate="no">.*}

      assert highlight("""
             ```html
             <foo />
             ```
             """) =~
               ~r{<pre><code class=\"makeup html\" translate="no">.*}

      assert highlight("""
             ```
             $ hello
             ```
             """) =~
               ~r{<pre><code class=\"makeup shell\" translate="no"><span class="gp unselectable">\$.*}

      assert highlight("""
             > ```elixir
             > hello
             > ```
             """) =~
               ~r{<blockquote><pre><code class=\"makeup elixir\" translate="no">.*}
    end

    defp highlight(markdown) do
      markdown
      |> DocAST.parse!("text/markdown")
      |> DocAST.highlight(ExDoc.Language.Elixir)
      |> DocAST.to_string()
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
             |> DocAST.parse!("text/markdown")
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
