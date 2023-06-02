otp_eep48? = Code.ensure_loaded?(:edoc_doclet_chunks)

exclude = [
  earmark: not ExDoc.Markdown.Earmark.available?(),
  otp_eep48: not otp_eep48?,
  otp_has_docs: not match?({:docs_v1, _, _, _, _, _, _}, Code.fetch_docs(:array))
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

    output_dir = context.tmp_dir <> "/html"
    File.mkdir_p!(output_dir)
    File.write!(output_dir <> "/.ex_doc", "")

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

    modules
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

    [module]
  end

  if otp_eep48? do
    def edoc_to_chunk(module) do
      source_path = module.module_info(:compile)[:source]
      dir = :filename.dirname(source_path)

      :ok =
        :edoc.files([source_path],
          preprocess: true,
          doclet: :edoc_doclet_chunks,
          layout: :edoc_layout_chunks,
          dir: dir ++ ~c"/doc"
        )

      module
    end
  else
    def edoc_to_chunk(_) do
      raise "not supported"
    end
  end
end
