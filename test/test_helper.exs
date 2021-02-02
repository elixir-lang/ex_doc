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
  def elixirc(context, filename \\ "nofile", code) do
    dir = context.tmp_dir

    src_path = Path.join([dir, filename])
    src_path |> Path.dirname() |> File.mkdir_p!()
    File.write!(src_path, code)

    ebin_dir = Path.join(dir, "ebin")
    File.mkdir_p!(ebin_dir)
    {:ok, modules, []} = Kernel.ParallelCompiler.compile_to_path([src_path], ebin_dir)
    Code.prepend_path(ebin_dir)

    ExUnit.Callbacks.on_exit(fn ->
      for module <- modules do
        :code.purge(module)
        :code.delete(module)
      end

      File.rm_rf!(dir)
    end)
  end

  # TODO: replace with ExUnit @tag :tmp_dir feature when we require Elixir v1.11.
  def create_tmp_dir(context) do
    module = escape_path(inspect(context.module))
    name = escape_path(to_string(context.test))
    dir = Path.join(["tmp", module, name])
    File.rm_rf!(dir)
    File.mkdir_p!(dir)

    Map.put(context, :tmp_dir, dir)
  end

  @escape Enum.map(' [~#%&*{}\\:<>?/+|"]', &<<&1::utf8>>)

  defp escape_path(path) do
    String.replace(path, @escape, "-")
  end
end
