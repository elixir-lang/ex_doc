exclude = [
  earmark: not ExDoc.Markdown.Earmark.available?(),
  otp23: System.otp_release() < "23",
  otp24: System.otp_release() < "24"
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
    true = Code.prepend_path(ebin_dir)

    ExUnit.Callbacks.on_exit(fn ->
      for module <- modules do
        :code.purge(module)
        :code.delete(module)
      end

      File.rm_rf!(dir)
    end)
  end

  def erlc(context, module, code, opts \\ []) do
    dir = context.tmp_dir

    src_path = Path.join([dir, "#{module}.erl"])
    src_path |> Path.dirname() |> File.mkdir_p!()
    File.write!(src_path, code)

    ebin_dir = Path.join(dir, "ebin")
    File.mkdir_p!(ebin_dir)

    {:ok, module} =
      :compile.file(String.to_charlist(src_path), [
        :return_errors,
        :debug_info,
        outdir: String.to_charlist(ebin_dir)
      ])

    true = Code.prepend_path(ebin_dir)
    {:module, ^module} = :code.load_file(module)

    ExUnit.Callbacks.on_exit(fn ->
      :code.purge(module)
      :code.delete(module)
      File.rm_rf!(dir)
    end)

    if Keyword.get(opts, :docs, true) do
      edoc_to_chunk(module)
    end

    :ok
  end

  if Code.ensure_loaded?(:edoc_doclet_chunks) do
    def edoc_to_chunk(module) do
      source_path = module.module_info(:compile)[:source]
      dir = :filename.dirname(source_path)

      :ok =
        :edoc.files([source_path],
          preprocess: true,
          doclet: :edoc_doclet_chunks,
          layout: :edoc_layout_chunks,
          dir: dir ++ '/doc'
        )
    end
  else
    def edoc_to_chunk(module) do
      source_path = module.module_info(:compile)[:source]
      beam_path = :code.which(module)
      dir = :filename.dirname(source_path)
      xml_path = '#{dir}/#{module}.xml'
      chunk_path = '#{dir}/#{module}.chunk'

      docgen_priv_dir = :code.priv_dir(:erl_docgen)
      cmd!("escript #{docgen_priv_dir}/bin/xml_from_edoc.escript -dir #{dir} #{source_path}")

      :docgen_xml_to_chunk.main(["app", xml_path, beam_path, "", chunk_path])
      docs_chunk = File.read!(chunk_path)
      {:ok, ^module, chunks} = :beam_lib.all_chunks(beam_path)
      {:ok, beam} = :beam_lib.build_module([{'Docs', docs_chunk} | chunks])
      File.write!(beam_path, beam)
    end

    defp cmd!(command) do
      0 = Mix.shell().cmd(command)
    end
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
