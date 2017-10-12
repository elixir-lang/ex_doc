defmodule ExDoc.Markdown.DummyMarkdownProcessor do
    @moduledoc false

    # This module is used for testing only.

    @behaviour ExDoc.MarkdownProcessor

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
