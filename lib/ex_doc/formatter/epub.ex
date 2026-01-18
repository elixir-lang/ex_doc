defmodule ExDoc.Formatter.EPUB do
  @moduledoc false

  @mimetype "application/epub+zip"
  @assets_dir "OEBPS/assets"
  alias __MODULE__.{Assets, Templates}
  alias ExDoc.{Formatter, Utils}

  @doc """
  Generates EPUB documentation for the given modules.
  """
  @spec run([ExDoc.ModuleNode.t()], list(), ExDoc.Config.t()) ::
          String.t()
  def run(project_nodes, extras, config) when is_map(config) do
    config = normalize_config(config)
    File.rm_rf!(config.output)
    File.mkdir_p!(Path.join(config.output, "OEBPS"))

    extras = Formatter.autolink_extras(extras, ".xhtml", config)

    project_nodes =
      Formatter.render_all(project_nodes, extras, ".xhtml", config,
        highlight_tag: "samp"
      )

    {modules, tasks} = Enum.split_with(project_nodes, &(&1.type != :task))

    static_files = Formatter.generate_assets("OEBPS", default_assets(config), config)
    Formatter.generate_logo(@assets_dir, config)
    Formatter.generate_cover(@assets_dir, config)

    generate_content(config, modules, tasks, extras, static_files)
    generate_nav(config, modules, tasks, extras)
    generate_title(config)
    generate_extras(extras, config)
    generate_list(config, modules)
    generate_list(config, tasks)

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

  defp generate_extras(extras, config) do
    for %ExDoc.Extras.Page{} = node <- extras do
      output = "#{config.output}/OEBPS/#{node.id}.xhtml"
      html = Templates.extra_template(config, node)

      if File.regular?(output) do
        Utils.warn("file #{Path.relative_to_cwd(output)} already exists", [])
      end

      File.write!(output, html)
    end
  end

  defp generate_content(config, modules, tasks, extras, static_files) do
    uuid = "urn:uuid:#{uuid4()}"
    datetime = format_datetime()

    static_files =
      for name <- static_files,
          String.contains?(name, "OEBPS"),
          media_type = Templates.media_type(Path.extname(name)),
          do: {Path.relative_to(name, "OEBPS"), media_type}

    content =
      Templates.content_template(config, modules, tasks, extras, uuid, datetime, static_files)

    File.write("#{config.output}/OEBPS/content.opf", content)
  end

  defp generate_nav(config, modules, tasks, extras) do
    modules = group_by_group(modules)
    tasks = group_by_group(tasks)
    extras = group_by_group(extras)

    content = Templates.nav_template(config, modules, tasks, extras)
    File.write("#{config.output}/OEBPS/nav.xhtml", content)
  end

  defp group_by_group(nodes) do
    nodes
    |> Enum.chunk_by(& &1.group)
    |> Enum.map(&{hd(&1).group, &1})
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
    content = Templates.module_template(config, module_node)
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
