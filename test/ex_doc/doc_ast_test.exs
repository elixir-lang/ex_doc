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

    test "markdown errors" do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert DocAST.parse!("`String.upcase", "text/markdown") ==
                        [{:p, [], ["`String.upcase"], %{}}]
             end) =~ "(warning) nofile:1 Closing unclosed backquotes ` at end of input"
    end

    test "erlang+html" do
      erl_ast = [{:p, [], ["foo"]}]

      assert DocAST.parse!(erl_ast, "application/erlang+html") ==
               [{:p, [], ["foo"], %{}}]
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
      assert DocAST.to_string(ast) == ~s{<span><i></i><i></i></span>}
    end

    test "with fun" do
      markdown = """
      foo **bar** baz
      """

      ast = DocAST.parse!(markdown, "text/markdown")

      f = fn
        {:strong, _, _, _}, string ->
          String.upcase(string)

        _ast, string ->
          string
      end

      assert DocAST.to_string(ast, f) ==
               "<p>foo <STRONG>BAR</STRONG> baz</p>"
    end

    test "void elements" do
      markdown = """
      foo  
      bar
      """

      ast = DocAST.parse!(markdown, "text/markdown")

      assert DocAST.to_string(ast) == ~s{<p>foo<br/>bar</p>}
    end
  end

  describe "highlight" do
    test "if no language is given, highlight as iex/elixir" do
      with_empty_class = ~S[<pre><code class="">mix run --no-halt path/to/file.exs</code></pre>]
      without_class = "<pre><code>mix run --no-halt path/to/file.exs</code></pre>"
      iex_detected_with_empty_class = ~S[<pre><code class="">iex&gt; max(4, 5)</code></pre>]
      iex_detected_without_class = ~S[<pre><code>iex&gt; max(4, 5)</code></pre>]

      assert DocAST.highlight(with_empty_class) =~
               ~r{<pre><code class=\"makeup elixir\">.*}

      assert DocAST.highlight(without_class) =~
               ~r{<pre><code class=\"makeup elixir\">.*}

      # IEx is highlighted by the normal elixir lexer
      assert DocAST.highlight(iex_detected_with_empty_class) =~
               ~r{<pre><code class=\"makeup elixir\">.*}

      assert DocAST.highlight(iex_detected_without_class) =~
               ~r{<pre><code class=\"makeup elixir\">.*}
    end
  end
end
