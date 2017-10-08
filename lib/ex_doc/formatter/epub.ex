defmodule ExDoc.Formatter.EPUB do
  @moduledoc """
  Generates EPUB documentation for Elixir projects.
  """

  @mimetype "application/epub+zip"
  alias __MODULE__.{Assets, Templates}
  alias ExDoc.Formatter.HTML

  @doc """
  Generate EPUB documentation for the given modules.
  """
  @spec run(list, ExDoc.Config.t) :: String.t
  def run(project_nodes, config) when is_map(config) do
    config = normalize_config(config)
    config = %{config | extras: project_nodes |> HTML.build_extras(config, ".xhtml") |> group_extras()}

    File.rm_rf!(config.output)
    File.mkdir_p!(Path.join(config.output, "OEBPS"))

    assets_dir = "OEBPS/assets"
    static_files = HTML.generate_assets(config, assets_dir, default_assets())
    HTML.generate_logo(assets_dir, config)

    all = HTML.Autolink.all(project_nodes, ".xhtml", config.deps)
    nodes_map = %{
      modules: HTML.filter_list(:module, all),
      exceptions: HTML.filter_list(:exception, all),
      tasks: HTML.filter_list(:task, all)
    }

    uuid = "urn:uuid:#{uuid4()}"
    datetime = format_datetime()

    generate_content(config, nodes_map, uuid, datetime, static_files)
    generate_nav(config, nodes_map)
    generate_title(config)
    generate_extras(config)
    generate_list(config, nodes_map.modules)
    generate_list(config, nodes_map.exceptions)
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
    for {_title, extras} <- config.extras do
      Enum.each(extras, fn %{id: id, title: title, content: content} ->
        output = "#{config.output}/OEBPS/#{id}.xhtml"
        html = Templates.extra_template(config, title, content)
        if File.regular? output do
          IO.puts :stderr, "warning: file #{Path.relative_to_cwd output} already exists"
        end
        File.write!(output, html)
      end)
    end
  end

  defp generate_content(config, nodes, uuid, datetime, static_files) do
    static_files =
      static_files
      |> Enum.filter(fn(name) ->
           String.contains?(name, "OEBPS") and config.output |> Path.join(name) |> File.regular?()
      end)
      |> Enum.map(&Path.relative_to(&1, "OEBPS"))

    content = Templates.content_template(config, nodes, uuid, datetime, static_files)
    File.write("#{config.output}/OEBPS/content.opf", content)
  end

  defp generate_nav(config, nodes) do
    content = Templates.nav_template(config, nodes)
    File.write("#{config.output}/OEBPS/nav.xhtml", content)
  end

  defp group_extras(extras) do
    {extras_by_group, groups} =
      extras
      |> Enum.with_index()
      |> Enum.reduce({%{}, %{}}, fn({x, index}, {extras_by_group, groups}) ->
           group = if x.group != "", do: x.group, else: "Extras"
           extras_by_group = Map.update(extras_by_group, group, [x], &([x | &1]))
           groups = Map.put_new(groups, group, index)
           {extras_by_group, groups}
         end)

    groups
    |> Map.to_list()
    |> List.keysort(1)
    |> Enum.map(fn({k, _}) -> {k, Enum.reverse(Map.get(extras_by_group, k))} end)
  end

  defp generate_title(config) do
    content = Templates.title_template(config)
    File.write("#{config.output}/OEBPS/title.xhtml", content)
  end

  defp generate_list(config, nodes) do
    nodes
    |> Enum.map(&Task.async(fn ->
         generate_module_page(config.output, config, &1)
       end))
    |> Enum.map(&Task.await(&1, :infinity))
  end

  defp generate_epub(output) do
    :zip.create(String.to_charlist("#{output}.epub"),
                [{'mimetype', @mimetype} | files_to_add(output)],
                compress: ['.css', '.xhtml', '.html', '.ncx', '.js',
                           '.opf', '.jpg', '.png', '.xml'])
  end

  ## Helpers

  defp default_assets do
    [{Assets.dist(), "OEBPS/dist"},
     {Assets.metainfo(), "META-INF"},
     # Implementation Note:
     # --------------------
     # In the EPUB format, extra files such as CSS and Javascript must
     # be included inside the OEBPS directory.
     # The file "OEBPS/file.ext" will be referenced inside pages as "file.ext".
     # This means that as long as the assets are included with the "OEBPS" prefix,
     # they will be accessible as if it were an HTML document.
     # This allows us to reuse pretty much all logic between the `:epub` and the `:html` formats.
     {Assets.markdown_processor_assets(), "OEBPS"}]
  end

  defp files_to_add(path) do
    Enum.reduce Path.wildcard(Path.join(path, "**/*")), [], fn file, acc ->
      case File.read(file) do
        {:ok, bin} ->
          [{file |> Path.relative_to(path) |> String.to_charlist(), bin} | acc]
        {:error, _} ->
          acc
      end
    end
  end

  # Helper to format Erlang datetime tuple
  defp format_datetime do
    {{year, month, day}, {hour, min, sec}} = :calendar.universal_time()
    list = [year, month, day, hour, min, sec]
    "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ"
    |> :io_lib.format(list)
    |> IO.iodata_to_binary()
  end

  defp generate_module_page(output, config, module_node) do
    content = Templates.module_page(config, module_node)
    File.write("#{output}/OEBPS/#{module_node.id}.xhtml", content)
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
