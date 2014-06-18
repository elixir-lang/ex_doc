defmodule ExDoc.Markdown do
  def to_html(text, header_lvl \\ 1) do
    ExDoc.get_markdown_processor().to_html(text, header_lvl)
  end
end
