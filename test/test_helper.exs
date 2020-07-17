exclude = [
  earmark: not ExDoc.Markdown.Earmark.available?()
]

ExUnit.start(exclude: Enum.filter(exclude, &elem(&1, 1)))

# Prepare module fixtures
File.rm_rf!("test/tmp")
File.mkdir_p!("test/tmp/beam")
File.mkdir_p!("test/tmp/beam/refs")
Code.prepend_path("test/tmp/beam")
Code.prepend_path("test/tmp/beam/refs")

# Compile module fixtures
"test/fixtures/*.ex"
|> Path.wildcard()
|> Kernel.ParallelCompiler.compile_to_path("test/tmp/beam")

# Compile Refs fixtures
"test/fixtures/refs/*.ex"
|> Path.wildcard()
|> Kernel.ParallelCompiler.compile_to_path("test/tmp/beam/refs")
