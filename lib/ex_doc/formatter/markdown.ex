defmodule ExDoc.Formatter.MARKDOWN do
  @moduledoc false

  alias __MODULE__.Templates

  def run(config, project_nodes, extras) when is_map(config) do
    File.mkdir_p!(config.output)

    {modules, tasks} =
      project_nodes
      |> Enum.filter(&(&1.source_format == "text/markdown"))
      |> Enum.split_with(&(&1.type != :task))

    groups =
      Map.new([modules: modules, mix_tasks: tasks, extras: extras], fn {k, v} ->
        {k, group_by_group(v)}
      end)

    all_files =
      generate_nav(config, groups) ++
        generate_api_reference(config, groups) ++
        generate_extras(extras, config) ++
        generate_list(config, modules) ++
        generate_list(config, tasks)

    entrypoint = config.output |> Path.join("llms.txt") |> Path.relative_to_cwd()
    %{entrypoint: entrypoint, build: List.flatten(all_files)}
  end

  defp normalize_output(output) do
    output
    |> String.replace("\r\n", "\n")
    |> String.replace(~r/\n{3,}/, "\n\n")
  end

  defp generate_nav(config, %{modules: modules, mix_tasks: mix_tasks, extras: extras}) do
    content =
      config
      |> Templates.llms_txt_template(modules, mix_tasks, extras)
      |> normalize_output()

    filename = "llms.txt"

    write!(config, filename, content)

    [filename]
  end

  defp generate_api_reference(%{api_reference: false}, _groups) do
    []
  end

  defp generate_api_reference(config, %{modules: modules, mix_tasks: mix_tasks}) do
    content =
      config
      |> Templates.api_reference_template(modules, mix_tasks)
      |> normalize_output()

    filename = "api-reference.md"

    write!(config, filename, content)

    [filename]
  end

  defp group_by_group(nodes) do
    nodes
    |> Enum.chunk_by(& &1.group)
    |> Enum.map(&{hd(&1).group, &1})
  end

  defp generate_extras(extras, config) do
    for %ExDoc.ExtraNode{id: id, source_doc: content} <- extras do
      filename = "#{id}.md"

      write!(config, filename, content)

      filename
    end
  end

  defp generate_list(config, nodes) do
    nodes
    |> Task.async_stream(&generate_module(&1, config), timeout: :infinity)
    |> Enum.map(&elem(&1, 1))
  end

  ## Helpers

  defp generate_module(module_node, config) do
    content =
      config
      |> Templates.module_template(module_node)
      |> normalize_output()

    filename = "#{module_node.id}.md"

    write!(config, filename, content)

    filename
  end

  defp write!(config, filename, content) do
    config.output
    |> Path.join(filename)
    |> File.write!(content)
  end
end
