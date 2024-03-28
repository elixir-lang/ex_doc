defmodule ExDoc.Formatter.EPUB do
  @moduledoc false

  @mimetype "application/epub+zip"
  alias __MODULE__.{Assets, Templates}
  alias ExDoc.Formatter.HTML

  @doc """
  Generate EPUB documentation for the given modules.
  """
  @spec run([ExDoc.ModuleNode.t()], [ExDoc.ModuleNode.t()], ExDoc.Config.t()) :: String.t()
  def run(project_nodes, filtered_modules, config) when is_map(config) do
    config = normalize_config(config)
    File.rm_rf!(config.output)
    File.mkdir_p!(Path.join(config.output, "OEBPS"))

    project_nodes =
      HTML.render_all(project_nodes, filtered_modules, ".xhtml", config, highlight_tag: "samp")

    nodes_map = %{
      modules: HTML.filter_list(:module, project_nodes),
      tasks: HTML.filter_list(:task, project_nodes)
    }

    extras =
      config
      |> HTML.build_extras(".xhtml")
      |> Enum.chunk_by(& &1.group)
      |> Enum.map(&{hd(&1).group, &1})

    config = %{config | extras: extras}

    assets_dir = "OEBPS/assets"
    static_files = HTML.generate_assets(config, assets_dir, default_assets(config))
    HTML.generate_logo(assets_dir, config)
    HTML.generate_cover(assets_dir, config)

    uuid = "urn:uuid:#{uuid4()}"
    datetime = format_datetime()

    generate_content(config, nodes_map, uuid, datetime, static_files)
    generate_nav(config, nodes_map)
    generate_title(config)
    generate_extras(config)
    generate_list(config, nodes_map.modules)
    generate_list(config, nodes_map.tasks)

    {:ok, epub} = generate_epub(config.output)
    File.rm_rf!(config.output)
    Path.relative_to_cwd(epub)
  end

  @doc """
  Helper that replaces anchor names and links that could potentially cause problems on EPUB documents

  This helper replaces all the `&` with `&amp;` found in anchors like
  `Kernel.xhtml#&&/2` or `<h2 id="&&&/2-examples" class="section-heading">...</h2>`

  These anchor names cause a fatal error while EPUB readers parse the files,
  resulting in truncated content.

  For more details, see: https://github.com/elixir-lang/ex_doc/issues/1851
  """
  def fix_anchors(content) do
    content
    |> String.replace(~r{id="&+/\d+[^"]*}, &String.replace(&1, "&", "&amp;"))
    |> String.replace(~r{href="[^#"]*#&+/\d+[^"]*}, &String.replace(&1, "&", "&amp;"))
  end

  defp normalize_config(config) do
    output =
      config.output
      |> Path.expand()
      |> Path.join("#{config.project}")

    %{config | output: output}
  end

  defp generate_extras(config) do
    for {_title, extras} <- config.extras do
      Enum.each(extras, fn %{id: id, title: title, title_content: title_content, content: content} ->
        output = "#{config.output}/OEBPS/#{id}.xhtml"

        html =
          config
          |> Templates.extra_template(title, title_content, content)
          |> fix_anchors()

        if File.regular?(output) do
          ExDoc.Utils.warn("file #{Path.relative_to_cwd(output)} already exists", [])
        end

        File.write!(output, html)
      end)
    end
  end

  defp generate_content(config, nodes, uuid, datetime, static_files) do
    static_files =
      static_files
      |> Enum.filter(fn name ->
        String.contains?(name, "OEBPS") and config.output |> Path.join(name) |> File.regular?()
      end)
      |> Enum.map(&Path.relative_to(&1, "OEBPS"))

    content = Templates.content_template(config, nodes, uuid, datetime, static_files)
    File.write("#{config.output}/OEBPS/content.opf", content)
  end

  defp generate_nav(config, nodes) do
    nodes =
      Map.update!(nodes, :modules, fn modules ->
        modules |> Enum.chunk_by(& &1.group) |> Enum.map(&{hd(&1).group, &1})
      end)

    content = Templates.nav_template(config, nodes)
    File.write("#{config.output}/OEBPS/nav.xhtml", content)
  end

  defp generate_title(config) do
    content = Templates.title_template(config)
    File.write("#{config.output}/OEBPS/title.xhtml", content)
  end

  defp generate_list(config, nodes) do
    nodes
    |> Task.async_stream(&generate_module_page(&1, config), timeout: :infinity)
    |> Enum.map(&elem(&1, 1))
  end

  defp generate_epub(output) do
    :zip.create(
      String.to_charlist("#{output}.epub"),
      [{~c"mimetype", @mimetype} | files_to_add(output)],
      compress: [
        ~c".css",
        ~c".xhtml",
        ~c".html",
        ~c".ncx",
        ~c".js",
        ~c".opf",
        ~c".jpg",
        ~c".png",
        ~c".xml"
      ]
    )
  end

  ## Helpers

  defp default_assets(config) do
    [
      {Assets.dist(config.proglang), "OEBPS/dist"},
      {Assets.metainfo(), "META-INF"}
    ]
  end

  defp files_to_add(path) do
    Enum.reduce(Path.wildcard(Path.join(path, "**/*")), [], fn file, acc ->
      case File.read(file) do
        {:ok, bin} ->
          [{file |> Path.relative_to(path) |> String.to_charlist(), bin} | acc]

        {:error, _} ->
          acc
      end
    end)
  end

  # Helper to format Erlang datetime tuple
  defp format_datetime do
    {{year, month, day}, {hour, min, sec}} = :calendar.universal_time()
    list = [year, month, day, hour, min, sec]

    "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ"
    |> :io_lib.format(list)
    |> IO.iodata_to_binary()
  end

  defp generate_module_page(module_node, config) do
    content =
      config
      |> Templates.module_page(module_node)
      |> fix_anchors()

    File.write("#{config.output}/OEBPS/#{module_node.id}.xhtml", content)
  end

  @two_power_16 65536
  @two_power_32 4_294_967_296
  @two_power_48 281_474_976_710_656

  defp uuid4 do
    Enum.map_join(
      [
        <<:rand.uniform(@two_power_32) - 1::32>>,
        <<:rand.uniform(@two_power_16) - 1::16>>,
        <<:rand.uniform(@two_power_16) - 1::16>>,
        <<:rand.uniform(@two_power_16) - 1::16>>,
        <<:rand.uniform(@two_power_48) - 1::48>>
      ],
      <<45>>,
      &Base.encode16(&1, case: :lower)
    )
  end
end
