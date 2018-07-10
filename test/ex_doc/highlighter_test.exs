defmodule ExDoc.HighlighterTest do
  use ExUnit.Case, async: true

  alias ExDoc.Highlighter

  test "if no language is given, highlight as iex/elixir" do
    with_empty_class = ~S[<pre><code class="">mix run --no-halt path/to/file.exs</code></pre>]
    without_class = "<pre><code>mix run --no-halt path/to/file.exs</code></pre>"
    iex_detected_with_empty_class = ~S[<pre><code class="">iex&gt; max(4, 5)</code></pre>]
    iex_detected_without_class = ~S[<pre><code>iex&gt; max(4, 5)</code></pre>]

    assert Highlighter.highlight_code_blocks(with_empty_class) =~
             ~r{<pre><code class=\"nohighlight makeup elixir\">.*}

    assert Highlighter.highlight_code_blocks(without_class) =~
             ~r{<pre><code class=\"nohighlight makeup elixir\">.*}

    # IEx is highlighted by the normal elixir lexer
    assert Highlighter.highlight_code_blocks(iex_detected_with_empty_class) =~
             ~r{<pre><code class=\"nohighlight makeup elixir\">.*}

    assert Highlighter.highlight_code_blocks(iex_detected_without_class) =~
             ~r{<pre><code class=\"nohighlight makeup elixir\">.*}
  end
end
