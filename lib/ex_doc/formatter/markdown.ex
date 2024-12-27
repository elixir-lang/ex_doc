defmodule ExDoc.Formatter.MARKDOWN do
  @moduledoc false

  alias __MODULE__.{Templates}
  alias ExDoc.Formatter
  alias ExDoc.Utils

  @doc """
  Generates Markdown documentation for the given modules.
  """
  @spec run([ExDoc.ModuleNode.t()], [ExDoc.ModuleNode.t()], ExDoc.Config.t()) :: String.t()
  def run(project_nodes, filtered_modules, config) when is_map(config) do
    Utils.unset_warned()

    config = normalize_config(config)
    File.rm_rf!(config.output)
    File.mkdir_p!(config.output)

    project_nodes =
      project_nodes
      # |> Enum.map(&elem(&1, 1))
      |> Formatter.render_all(filtered_modules, ".md", config, highlight_tag: "samp")

    nodes_map = %{
      modules: Formatter.filter_list(:module, project_nodes),
      tasks: Formatter.filter_list(:task, project_nodes)
    }

    extras =
      config
      |> Formatter.build_extras(".md")
      |> Enum.chunk_by(& &1.group)
      |> Enum.map(&{hd(&1).group, &1})

    config = %{config | extras: extras}

    generate_nav(config, nodes_map)
    generate_extras(config)
    generate_list(config, nodes_map.modules)
    generate_list(config, nodes_map.tasks)

    config.output |> Path.join("index.md") |> Path.relative_to_cwd()
  end

  defp normalize_config(config) do
    output =
      config.output
      |> Path.expand()
      |> Path.join("markdown")

    %{config | output: output}
  end

  defp normalize_output(output) do
    output
    |> String.replace(~r/\r\n|\r|\n/, "\n")
    |> String.replace(~r/\n{3,}/, "\n\n")
  end

  defp generate_nav(config, nodes) do
    nodes =
      Map.update!(nodes, :modules, fn modules ->
        modules |> Enum.chunk_by(& &1.group) |> Enum.map(&{hd(&1).group, &1})
      end)

    content =
      Templates.nav_template(config, nodes)
      |> normalize_output()

    File.write("#{config.output}/index.md", content)
  end

  defp generate_extras(config) do
    for {_title, extras} <- config.extras do
      Enum.each(extras, fn %{id: id, source: content} ->
        output = "#{config.output}/#{id}.md"

        if File.regular?(output) do
          Utils.warn("file #{Path.relative_to_cwd(output)} already exists", [])
        end

        File.write!(output, normalize_output(content))
      end)
    end
  end

  defp generate_list(config, nodes) do
    nodes
    |> Task.async_stream(&generate_module_page(&1, config), timeout: :infinity)
    |> Enum.map(&elem(&1, 1))
  end

  ## Helpers

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

  defp generate_module_page(module_node, config) do
    content =
      Templates.module_page(config, module_node)
      |> normalize_output()

    File.write("#{config.output}/#{module_node.id}.md", content)
  end
end
