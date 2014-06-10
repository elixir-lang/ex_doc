defmodule ExDoc.Markdown do
  def to_html(text, header_lvl \\ 1) do
    # Return the result of the first function call that succeeds
    candidates = [ExDoc.Markdown.Sundown, ExDoc.Markdown.Pandoc]
    Enum.find_value(candidates, & &1.to_html(text, header_lvl))
  end
end
