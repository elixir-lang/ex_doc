defmodule ExDoc.Formatter.EPUB do
  @moduledoc """
  Provide EPUB documentation
  """

  @mimetype "application/epub+zip"
  alias ExDoc.Formatter.HTML
  alias ExDoc.Formatter.EPUB.Templates

  @doc """
  Generate EPUB documentation for the given modules
  """
  @spec run(list, ExDoc.Config.t) :: String.t
  def run(module_nodes, config) when is_map(config) do
    output =
      config.output
      |> Path.expand()
      |> Path.join("#{config.project}-v#{config.version}")

    File.rm_rf!(output)
    File.mkdir_p!(Path.join(output, "OEBPS"))

    HTML.generate_assets(output, assets(config))
    HTML.generate_logo("OEBPS/assets", config)
    generate_mimetype(output)
    config = normalize_config(config)
    generate_extras(output, config, module_nodes)

    all = HTML.Autolink.all(module_nodes, ".xhtml", config.deps)
    modules = HTML.filter_list(:modules, all)
    exceptions = HTML.filter_list(:exceptions, all)
    protocols = HTML.filter_list(:protocols, all)

    uuid = "urn:uuid:#{uuid4()}"
    datetime = format_datetime()
    nodes = modules ++ exceptions ++ protocols

    generate_content(output, config, nodes, uuid, datetime)
    generate_toc(output, config, nodes, uuid)
    generate_nav(output, config, nodes)
    generate_title(output, config)
    generate_list(output, config, modules)
    generate_list(output, config, exceptions)
    generate_list(output, config, protocols)

    {:ok, epub_file} = generate_epub(output)
    File.rm_rf!(output)
    epub_file
  end

  defp generate_mimetype(output) do
    File.write("#{output}/mimetype", @mimetype)
  end

  defp normalize_config(config) do
    extras = Enum.into(config.extras, [], &normalize_extra(&1))
    Map.put(config, :extras, extras)
  end

  defp normalize_extra({input_file, options}) do
    input_file = to_string(input_file)
    filename = options[:filename] || input_file |> HTML.input_to_title() |> HTML.title_to_filename()

    %{
      title: options[:title],
      input: input_file,
      output: "OEBPS/#{filename}.xhtml",
      filename: filename,
      group: options[:group]
    }
  end

  defp normalize_extra(input) do
    filename = input |> HTML.input_to_title() |> HTML.title_to_filename()

    %{
      title: HTML.input_to_title(input),
      input: input,
      output: "OEBPS/#{filename}.xhtml",
      filename: filename,
      group: ""
    }
  end

  defp generate_extras(output, config, module_nodes) do
    config.extras
    |> Enum.map(&Task.async(fn ->
         create_extra_files(&1, output, config, module_nodes)
       end))
    |> Enum.map(&Task.await(&1, :infinity))
  end

  defp create_extra_files(options, output, config, module_nodes) do
    if HTML.valid_extension_name?(options.input) do
      content =
        options.input
        |> File.read!()
        |> HTML.Autolink.project_doc(module_nodes, nil, ".xhtml")

      html_content = ExDoc.Markdown.to_html(content, file: options.input, line: 1)
      title = options.title || HTML.extract_title(html_content) || HTML.input_to_title(options[:input])

      config = Map.put(config, :title, title)
      html = Templates.extra_template(config, html_content)
      output = "#{output}/#{options.output}"

      if File.regular? output do
        IO.puts "warning: file #{Path.relative_to_cwd output} already exists"
      end

      File.write!(output, html)
    else
      raise ArgumentError, "file format not recognized, allowed format is: .md"
    end
  end

  defp generate_content(output, config, nodes, uuid, datetime) do
    content = Templates.content_template(config, nodes, uuid, datetime)
    File.write("#{output}/OEBPS/content.opf", content)
  end

  defp generate_toc(output, config, nodes, uuid) do
    content = Templates.toc_template(config, nodes, uuid)
    File.write("#{output}/OEBPS/toc.ncx", content)
  end

  defp generate_nav(output, config, nodes) do
    content = Templates.nav_template(config, nodes)
    File.write("#{output}/OEBPS/nav.xhtml", content)
  end

  defp generate_title(output, config) do
    content = Templates.title_template(config)
    File.write("#{output}/OEBPS/title.xhtml", content)
  end

  defp generate_list(output, config, nodes) do
    nodes
    |> Enum.map(&Task.async(fn ->
         generate_module_page(output, config, &1)
       end))
    |> Enum.map(&Task.await(&1, :infinity))
  end

  defp generate_epub(output) do
    :zip.create(String.to_char_list("#{output}.epub"),
                files_to_add(output),
                cwd: output,
                compress: ['.css', '.xhtml', '.html', '.ncx',
                           '.opf', '.jpg', '.png', '.xml'])
  end

  ## Helpers

  defp assets(%{assets: nil}), do: assets()
  defp assets(%{assets: path}), do: [{path, "OEBPS/assets"} | assets()]
  defp assets do
   [{assets_path("dist"), "OEBPS/dist"},
    {assets_path("assets"), "META-INF"}]
  end

  defp assets_path(pattern) do
    Application.app_dir(:ex_doc, "priv/ex_doc/formatter/epub/templates/#{pattern}")
  end

  defp files_to_add(path) do
    path
    |> Path.join("**/*")
    |> Path.wildcard()
    |> Enum.map(& &1 |> Path.relative_to(path) |> String.to_char_list())
  end

  # Helper to format Erlang datetime tuple
  defp format_datetime do
    {{year, month, day}, {hour, min, sec}} = :calendar.universal_time()
    list = [year, month, day, hour, min, sec]
    "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ"
    |> :io_lib.format(list)
    |> IO.iodata_to_binary()
  end

  defp generate_module_page(output, config, node) do
    content = Templates.module_page(config, node)
    File.write("#{output}/OEBPS/#{node.id}.xhtml", content)
  end

  # Helper to generate an UUID v4. This version uses pseudo-random bytes generated by
  # the `crypto` module.
  defp uuid4 do
    <<u0::48, _::4, u1::12, _::2, u2::62>> = :crypto.strong_rand_bytes(16)
    bin = <<u0::48, 4::4, u1::12, 2::2, u2::62>>
    <<u0::32, u1::16, u2::16, u3::16, u4::48>> = bin

    Enum.map_join([<<u0::32>>, <<u1::16>>, <<u2::16>>, <<u3::16>>, <<u4::48>>], <<45>>,
                  &(Base.encode16(&1, case: :lower)))
  end
end
