defmodule ExDoc.Formatter.EPUB do
  @moduledoc false

  @mimetype "application/epub+zip"
  @assets_dir "OEBPS/assets"
  alias __MODULE__.{Assets, Templates}
  alias ExDoc.{Formatter, Utils}

  @doc """
  Generates EPUB documentation for the given modules.
  """
  @spec run([ExDoc.ModuleNode.t()], [ExDoc.ModuleNode.t()], ExDoc.Config.t()) :: String.t()
  def run(project_nodes, filtered_modules, config) when is_map(config) do
    config = normalize_config(config)
    File.rm_rf!(config.output)
    File.mkdir_p!(Path.join(config.output, "OEBPS"))

    project_nodes =
      Formatter.render_all(project_nodes, filtered_modules, ".xhtml", config,
        highlight_tag: "samp"
      )

    nodes_map = %{
      modules: Formatter.filter_list(:module, project_nodes),
      tasks: Formatter.filter_list(:task, project_nodes)
    }

    extras =
      config
      |> Formatter.build_extras(".xhtml")
      |> Enum.chunk_by(& &1.group)
      |> Enum.map(&{hd(&1).group, &1})

    config = %{config | extras: extras}

    static_files = Formatter.generate_assets("OEBPS", default_assets(config), config)
    Formatter.generate_logo(@assets_dir, config)
    Formatter.generate_cover(@assets_dir, config)

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

  defp normalize_config(config) do
    output =
      config.output
      |> Path.expand()
      |> Path.join("#{config.project}")

    %{config | output: output}
  end

  defp generate_extras(config) do
    for {_title, extras} <- config.extras,
        node <- extras,
        not is_map_key(node, :url) do
      output = "#{config.output}/OEBPS/#{node.id}.xhtml"
      html = Templates.extra_template(config, node)

      if File.regular?(output) do
        Utils.warn("file #{Path.relative_to_cwd(output)} already exists", [])
      end

      File.write!(output, html)
    end
  end

  defp generate_content(config, nodes, uuid, datetime, static_files) do
    static_files =
      for name <- static_files,
          String.contains?(name, "OEBPS"),
          media_type = Templates.media_type(Path.extname(name)),
          do: {Path.relative_to(name, "OEBPS"), media_type}

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
    content = Templates.module_page(config, module_node)
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
