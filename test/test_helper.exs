exclude = [
  pandoc:  !ExDoc.Markdown.Pandoc.available?,
  hoedown: !ExDoc.Markdown.Hoedown.available?,
  earmark: !ExDoc.Markdown.Earmark.available?
]

ExUnit.start(exclude: Enum.filter(exclude, &elem(&1, 1)))
