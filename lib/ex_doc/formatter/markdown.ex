defmodule ExDoc.Formatter.Markdown do
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

    extras = Formatter.build_extras(config, ".md")

    project_nodes =
      project_nodes
      |> Formatter.render_all(filtered_modules, ".md", config, highlight_tag: "samp")

    nodes_map = %{
      modules: Formatter.filter_list(:module, project_nodes),
      tasks: Formatter.filter_list(:task, project_nodes)
    }

    config = %{config | extras: extras}

    generate_nav(config, nodes_map)
    generate_extras(config)
    generate_list(config, nodes_map.modules)
    generate_list(config, nodes_map.tasks)
    generate_llm_index(config, nodes_map)

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
    |> String.replace(["\r\n", "\n"], "\n")
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

  defp generate_module_page(module_node, config) do
    content =
      Templates.module_page(config, module_node)
      |> normalize_output()

    File.write("#{config.output}/#{module_node.id}.md", content)
  end

  defp generate_llm_index(config, nodes_map) do
    content = generate_llm_index_content(config, nodes_map)
    File.write("#{config.output}/llms.txt", content)
  end

  defp generate_llm_index_content(config, nodes_map) do
    project_info = """
    # #{config.project} #{config.version}

    #{config.project} documentation index for Large Language Models.

    ## Modules

    """

    modules_info =
      nodes_map.modules
      |> Enum.map(fn module_node ->
        "- **#{module_node.title}** (#{module_node.id}.md): #{module_node.doc |> ExDoc.DocAST.synopsis() |> extract_plain_text()}"
      end)
      |> Enum.join("\n")

    tasks_info =
      if length(nodes_map.tasks) > 0 do
        tasks_list =
          nodes_map.tasks
          |> Enum.map(fn task_node ->
            "- **#{task_node.title}** (#{task_node.id}.md): #{task_node.doc |> ExDoc.DocAST.synopsis() |> extract_plain_text()}"
          end)
          |> Enum.join("\n")

        "\n\n## Mix Tasks\n\n" <> tasks_list
      else
        ""
      end

    extras_info =
      if is_list(config.extras) and length(config.extras) > 0 do
        extras_list =
          config.extras
          |> Enum.flat_map(fn
            {_group, extras} when is_list(extras) -> extras
            _ -> []
          end)
          |> Enum.map(fn extra ->
            "- **#{extra.title}** (#{extra.id}.md): #{extra.title}"
          end)
          |> Enum.join("\n")

        if extras_list == "" do
          ""
        else
          "\n\n## Guides\n\n" <> extras_list
        end
      else
        ""
      end

    project_info <> modules_info <> tasks_info <> extras_info
  end

  defp extract_plain_text(html) when is_binary(html) do
    html
    |> String.replace(~r/<[^>]*>/, "")
    |> String.replace(~r/\s+/, " ")
    |> String.trim()
    |> case do
      "" ->
        "No documentation available"

      text ->
        text
        |> String.slice(0, 150)
        |> then(fn s -> if String.length(s) == 150, do: s <> "...", else: s end)
    end
  end

  defp extract_plain_text(_), do: "No documentation available"
end
