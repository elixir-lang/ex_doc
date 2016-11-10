exclude = [
  cmark:   !ExDoc.Markdown.Cmark.available?,
  earmark: !ExDoc.Markdown.Earmark.available?,
  hoedown: !ExDoc.Markdown.Hoedown.available?,
  pandoc:  !ExDoc.Markdown.Pandoc.available?
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
