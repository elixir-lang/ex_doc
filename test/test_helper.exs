exclude = [
  pandoc:  !ExDoc.Markdown.Pandoc.available?,
  hoedown: !ExDoc.Markdown.Hoedown.available?,
  earmark: !ExDoc.Markdown.Earmark.available?
]

ExUnit.start(exclude: Enum.filter(exclude, &elem(&1, 1)))

File.rm_rf!("test/tmp")
File.mkdir_p!("test/tmp")
for fixture <- ~w(elixir.png README.md) do
  File.cp!("test/fixtures/#{fixture}", "test/tmp/#{fixture}")
end
