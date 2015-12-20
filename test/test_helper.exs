exclude = [
  cmark:   !ExDoc.Markdown.Cmark.available?,
  earmark: !ExDoc.Markdown.Earmark.available?,
  hoedown: !ExDoc.Markdown.Hoedown.available?,
  pandoc:  !ExDoc.Markdown.Pandoc.available?
]

ExUnit.start(exclude: Enum.filter(exclude, &elem(&1, 1)))
