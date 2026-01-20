defmodule ExDoc.Formatter.MARKDOWN do
  @moduledoc false

  alias __MODULE__.Templates

  def run(config, project_nodes, extras) when is_map(config) do
    File.mkdir_p!(config.output)

    {modules, tasks} =
      project_nodes
      |> Enum.filter(&(&1.source_format == "text/markdown"))
      |> Enum.split_with(&(&1.type != :task))

    all_files =
      generate_nav(config, modules, tasks, extras) ++
        generate_api_reference(config, modules, tasks) ++
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

  defp generate_nav(config, modules, tasks, extras) do
    modules = group_by_group(modules)
    mix_tasks = group_by_group(tasks)
    extras = group_by_group(extras)

    content =
      Templates.nav_template(config, modules, mix_tasks, extras, "Table of Contents")
      |> normalize_output()

    filename = "llms.txt"
    File.write(Path.join(config.output, filename), content)
    [filename]
  end

  defp generate_api_reference(%{api_reference: false}, _modules, _tasks) do
    []
  end

  defp generate_api_reference(config, modules, tasks) do
    modules = group_by_group(modules)
    mix_tasks = group_by_group(tasks)

    content =
      Templates.nav_template(config, modules, mix_tasks, [], "API Reference")
      |> normalize_output()

    filename = "api-reference.md"
    File.write(Path.join(config.output, filename), content)
    [filename]
  end

  defp group_by_group(nodes) do
    nodes
    |> Enum.chunk_by(& &1.group)
    |> Enum.map(&{hd(&1).group, &1})
  end

  defp generate_extras(extras, config) do
    for %ExDoc.ExtraNode{id: id, source_doc: source_doc} <- extras do
      filename = "#{id}.md"
      output = Path.join(config.output, filename)
      File.write!(output, source_doc)
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
      Templates.module_template(config, module_node)
      |> normalize_output()

    filename = "#{module_node.id}.md"
    File.write(Path.join(config.output, filename), content)
    filename
  end
end
