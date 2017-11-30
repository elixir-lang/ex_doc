exclude = [
  cmark: !ExDoc.Markdown.Cmark.available?,
  earmark: !ExDoc.Markdown.Earmark.available?,
  formatter: !function_exported?(Code, :format_string!, 2),
  no_formatter: function_exported?(Code, :format_string!, 2)
]

ExUnit.start(exclude: Enum.filter(exclude, &elem(&1, 1)))

# Prepare module fixtures
File.rm_rf!("test/tmp")
File.mkdir_p!("test/tmp/beam")
Code.prepend_path("test/tmp/beam")

# Compile module fixtures
"test/fixtures/*.ex"
|> Path.wildcard()
|> Kernel.ParallelCompiler.files_to_path("test/tmp/beam")

defmodule ExDoc.Markdown.DummyProcessor do
  @moduledoc false
  @behaviour ExDoc.Markdown

  def to_html(text, opts),
    do: ExDoc.Markdown.Earmark.to_html(text, opts)

  def assets(:html), do: [
    {"html_assets.css", "HTML assets - CSS"},
    {"html_assets.js", "HTML assets - Javascript"}]

  def assets(:epub), do: [
    {"epub_assets-css.css", "EPUB assets - CSS"},
    {"epub_assets-js.js", "EPUB assets - Javascript"}]

  def available?(), do: true

  def configure(_), do: :ok

  def before_closing_head_tag(:html),
    do: "UNIQUE:<dont-escape>&copy;MARKDOWN-PROCESSOR-BEFORE-CLOSING-HEAD-TAG-HTML</dont-escape>"
  def before_closing_head_tag(:epub),
    do: "UNIQUE:<dont-escape>&copy;MARKDOWN-PROCESSOR-BEFORE-CLOSING-HEAD-TAG-EPUB</dont-escape>"

  def before_closing_body_tag(:html),
    do: "UNIQUE:<dont-escape>&copy;MARKDOWN-PROCESSOR-BEFORE-CLOSING-BODY-TAG-HTML</dont-escape>"
  def before_closing_body_tag(:epub),
    do: "UNIQUE:<dont-escape>&copy;MARKDOWN-PROCESSOR-BEFORE-CLOSING-BODY-TAG-EPUB</dont-escape>"
end
