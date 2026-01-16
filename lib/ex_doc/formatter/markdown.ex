defmodule ExDoc.Formatter.MARKDOWN do
  @moduledoc false

  alias __MODULE__.Templates
  alias ExDoc.Formatter
  alias ExDoc.Utils

  @doc """
  Generates Markdown documentation for the given modules.
  """
  @spec run([ExDoc.ModuleNode.t()], [ExDoc.ModuleNode.t()], ExDoc.Config.t()) :: String.t()
  def run(project_nodes, filtered_modules, config) when is_map(config) do
    Utils.unset_warned()

    config = normalize_config(config)

    build = Path.join(config.output, ".build")
    output_setup(build, config)

    extras = Formatter.build_extras(config, ".md")

    project_nodes =
      Formatter.render_all(project_nodes, filtered_modules, ".md", config, highlight_tag: "samp")

    nodes_map = %{
      modules: Formatter.filter_list(:module, project_nodes),
      tasks: Formatter.filter_list(:task, project_nodes)
    }

    config = %{config | extras: extras}

    [
      generate_nav(config, nodes_map),
      generate_extras(config),
      generate_list(config, nodes_map.modules),
      generate_list(config, nodes_map.tasks),
      generate_llm_index(config, nodes_map)
    ]
    |> List.flatten()
    |> generate_build(build)

    config.output |> Path.join("index.md") |> Path.relative_to_cwd()
  end

  defp normalize_config(config) do
    output = Path.expand(config.output)
    %{config | output: output}
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
      |> Enum.sort()
      |> Enum.dedup()
      |> Enum.intersperse("\n")

    File.write!(build, entries)
  end

  defp normalize_output(output) do
    output
    |> String.replace(~r/\r\n?/, "\n")
    |> String.replace(~r/\n{3,}/, "\n\n")
  end

  defp generate_nav(config, nodes) do
    nodes =
      Map.update!(nodes, :modules, fn modules ->
        modules |> Enum.chunk_by(& &1.group) |> Enum.map(&{hd(&1).group, &1})
      end)

    content =
      config
      |> Templates.nav_template(nodes)
      |> normalize_output()

    filename = "index.md"

    config.output
    |> Path.join(filename)
    |> File.write(content)

    filename
  end

  defp generate_extras(config) do
    for {_title, extras} <- config.extras,
        %{id: id, source: content} <- extras,
        not is_map_key(%{id: id, source: content}, :url) do
      filename = "#{id}.md"
      output = Path.join(config.output, filename)

      if File.regular?(output),
        do: Utils.warn("file #{Path.relative_to_cwd(output)} already exists", [])

      File.write!(output, normalize_output(content))

      filename
    end
  end

  defp generate_list(config, nodes) do
    nodes
    |> Task.async_stream(&generate_module_page(&1, config), timeout: :infinity)
    |> Enum.map(&elem(&1, 1))
  end

  ## Helpers

  defp generate_module_page(module_node, config) do
    content =
      config
      |> Templates.module_page(module_node)
      |> normalize_output()

    filename = "#{module_node.id}.md"

    config.output
    |> Path.join(filename)
    |> File.write(content)

    filename
  end

  defp generate_llm_index(config, nodes_map) do
    content = generate_llm_index_content(config, nodes_map)
    filename = "llms.txt"

    config.output
    |> Path.join(filename)
    |> File.write(content)

    filename
  end

  defp generate_llm_index_content(config, nodes_map) do
    project_info = """
    # #{config.project} #{config.version}

    #{config.project} documentation index for Large Language Models.

    ## Modules

    """

    modules_info =
      Enum.map(nodes_map.modules, fn module_node ->
        synopsis = synopsis(module_node.doc)

        ["- [#{module_node.title}](#{module_node.id}.md): ", synopsis, "\n"]
      end)

    tasks_info =
      if Enum.any?(nodes_map.tasks) do
        tasks_list =
          Enum.map(nodes_map.tasks, fn task_node ->
            synopsis = synopsis(task_node.doc)

            ["- [#{task_node.title}](#{task_node.id}.md): ", synopsis, "\n"]
          end)

        ["\n## Mix Tasks\n\n" | tasks_list]
      else
        []
      end

    extras_info =
      if is_list(config.extras) and Enum.any?(config.extras) do
        extras_list =
          config.extras
          |> Enum.flat_map(fn
            {_group, extras} when is_list(extras) -> extras
            _ -> []
          end)
          |> Enum.map(&["- [#{&1.title}](#{&1.id}.md)", "\n"])

        if Enum.any?(extras_list) do
          ["\n## Guides\n\n" | extras_list]
        else
          []
        end
      else
        []
      end

    [project_info, modules_info, tasks_info, extras_info]
  end

  defp synopsis(doc) do
    doc
    |> ExDoc.DocAST.synopsis()
    |> extract_plain_text()
  end

  defp extract_plain_text(""), do: "No documentation available"

  defp extract_plain_text(html) when is_binary(html) do
    html =
      html
      |> String.replace(~r/<[^>]*>/, "")
      |> String.replace(~r/\s+/, " ")
      |> String.trim()

    case html do
      "" ->
        "No documentation available"

      text ->
        if String.length(text) > 150 do
          String.slice(text, 0, 150) <> "..."
        else
          text
        end
    end
  end
end
