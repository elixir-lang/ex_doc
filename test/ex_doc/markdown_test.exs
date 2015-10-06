defmodule ExDoc.MarkdownTest do
  use ExUnit.Case, async: true

  alias ExDoc.Markdown, as: Markdown

  test "add iex/elixir to markdown codeblocks" do
    with_empty_class = ~S[<pre><code class="">mix run --no-halt path/to/file.exs]
    without_class = "<pre><code>mix run --no-halt path/to/file.exs"
    iex_detected_with_empty_class = ~S[<pre><code class="">iex&gt; max(4, 5)]
    iex_detected_without_class = ~S[<pre><code>iex&gt; max(4, 5)]

    assert Markdown.pretty_codeblocks(with_empty_class) ==
           ~S[<pre><code class="elixir">mix run --no-halt path/to/file.exs]
    assert Markdown.pretty_codeblocks(without_class) ==
           ~S[<pre><code class="elixir">mix run --no-halt path/to/file.exs]
    assert Markdown.pretty_codeblocks(iex_detected_with_empty_class) ==
           ~S[<pre><code class="iex elixir">iex&gt; max(4, 5)]
    assert Markdown.pretty_codeblocks(iex_detected_without_class) ==
           ~S[<pre><code class="iex elixir">iex&gt; max(4, 5)]
  end
end
