defmodule ExDoc.Formatter.MARKDOWN do
  @moduledoc false

  alias __MODULE__.{Templates}
  alias ExDoc.Formatter
  alias ExDoc.Utils

  @doc """
  Generates Markdown documentation for the given modules.
  """
  @spec run([ExDoc.ModuleNode.t()], [ExDoc.ModuleNode.t()], ExDoc.Config.t()) :: String.t()
  def run(project_nodes, _filtered_modules, config) when is_map(config) do
    Utils.unset_warned()

    build = Path.join(config.output, ".build")
    output_setup(build, config)

    extras = Formatter.build_extras(config, ".md")
    config = %{config | extras: extras}

    {modules, tasks} =
      project_nodes
      |> Enum.filter(&(&1.source_format == "text/markdown"))
      |> Enum.split_with(&(&1.type != :task))

    all_files =
      [generate_nav(config, modules, tasks)] ++
        generate_extras(config) ++
        generate_list(config, modules) ++
        generate_list(config, tasks)

    generate_build(List.flatten(all_files), build)
    config.output |> Path.join("index.md") |> Path.relative_to_cwd()
  end

  defp output_setup(build, config) do
    if File.exists?(build) do
      build
      |> File.read!()
      |> String.split("\n", trim: true)
      |> Enum.map(&Path.join(config.output, &1))
      |> Enum.each(&File.rm/1)

      File.rm(build)
    else
      # Only remove markdown files, not HTML/EPUB files
      File.mkdir_p!(config.output)

      if File.exists?(config.output) do
        config.output
        |> Path.join("*.md")
        |> Path.wildcard()
        |> Enum.each(&File.rm/1)

        llms_file = Path.join(config.output, "llms.txt")
        if File.exists?(llms_file), do: File.rm(llms_file)
      end
    end
  end

  defp generate_build(files, build) do
    entries =
      files
      |> Enum.uniq()
      |> Enum.sort()
      |> Enum.map(&[&1, "\n"])

    File.mkdir_p!(Path.dirname(build))
    File.write!(build, entries)
  end

  defp normalize_output(output) do
    output
    |> String.replace("\r\n", "\n")
    |> String.replace(~r/\n{3,}/, "\n\n")
  end

  defp generate_nav(config, modules, tasks) do
    modules = group_by_group(modules)
    mix_tasks = group_by_group(tasks)
    extras = group_by_group(config.extras)

    content =
      Templates.nav_template(config, modules, mix_tasks, extras)
      |> normalize_output()

    filename = "index.md"
    File.write("#{config.output}/#{filename}", content)
    filename
  end

  defp group_by_group(nodes) do
    nodes
    |> Enum.chunk_by(& &1.group)
    |> Enum.map(&{hd(&1).group, &1})
  end

  defp generate_extras(config) do
    for %{id: id, source: source} = extra <- config.extras,
        not is_map_key(extra, :url) do
      filename = "#{id}.md"
      output = "#{config.output}/#{filename}"
      File.write!(output, source)
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
    File.write("#{config.output}/#{filename}", content)
    filename
  end
end
