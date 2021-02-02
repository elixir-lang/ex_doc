exclude = [
  earmark: not ExDoc.Markdown.Earmark.available?()
]

ExUnit.start(exclude: Enum.filter(exclude, &elem(&1, 1)))

# Prepare module fixtures
File.rm_rf!("test/tmp")
File.mkdir_p!("test/tmp/beam")
Code.prepend_path("test/tmp/beam")

# Compile module fixtures
"test/fixtures/*.ex"
|> Path.wildcard()
|> Kernel.ParallelCompiler.compile_to_path("test/tmp/beam")

defmodule TestHelper do
  def elixirc(filename \\ "nofile", code) do
    dir = tmp_dir(code)
    true = Code.prepend_path(dir)

    for {module, bytecode} <- Code.compile_string(code, filename) do
      beam_path = "#{dir}/#{module}.beam"
      File.write!(beam_path, bytecode)

      ExUnit.Callbacks.on_exit(fn ->
        :code.purge(module)
        :code.delete(module)
      end)
    end

    ExUnit.Callbacks.on_exit(fn ->
      Code.delete_path(dir)
      File.rm_rf!(dir)
    end)

    :ok
  end

  defp tmp_dir(code) do
    hash = :crypto.hash(:sha256, code) |> Base.url_encode64(case: :lower)
    dir = Path.join([File.cwd!(), "tmp", hash])
    File.mkdir_p!(dir)
    String.to_charlist(dir)
  end
end
