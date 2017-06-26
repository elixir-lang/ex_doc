defmodule ExDoc.Formatter.JSON do
  @moduledoc """
  Generate JSON documentation for Elixir projects

  The ExDoc JSON formatter starts with some information at the top:

  ## Top-level information

  * `about` - Indicates the version of the JSON structure format.
  * `name` - Project name
  * `version` - Project version
  * `description` - Project summary description
  * `homepage_url` - Specifies the project's home page
  * `language` - Identifies the primary language of the documents.
  * `icon` - Identifies the URL of the project's logo
  * `items` - This JSON object contains modules, exceptions, protocols,
    Mix tasks, extras, and attachments details.

  ## Modules, Exceptions, Protocols, Mix tasks

  Each component is an array that includes:

  * `module` - Module name
  * `title` - Module title
  * `doc` - Module documentation summary
  * `doc_line` - Line number where the module documentation starts
  * `source_path` - Path to the source code file in the project
  * `source_url` - URL to the source code
  * `types` - List of types
  * `functions` - List of functions
  * `callbacks` - List of callbacks

  ### Function, callback, and type details

  * `name` - Function name
  * `arity`- Function arity
  * `defaults` - Default argument values
  * `doc` - Function documentation
  * `doc_line` - Line number where the module documentation starts
  * `source_path` - Path to the source code file in the project
  * `source_url` - URL to the source code
  * `signature` - Indicates the function signature
  * `annotations` - Show annotations

  ## Extras

  This JSON object include the following fields:

  * `id` - Identifier
  * `title` - Document title
  * `group` - Specifies the group
  * `content` - The document content in HTML format

  ## Attachments

  * `path` - Relative path
  * `title` - Attachment title
  * `size_in_bytes` - File size in bytes

  """

  alias Mix.Project
  alias ExDoc.Formatter.HTML

  @spec run(list, ExDoc.Config.t) :: String.t
  def run(project_nodes, config) when is_map(config) do
    config =
      config
      |> normalize_config()
      |> output_setup()

    config
    |> create_project_node(project_nodes)
    |> Poison.encode!()
    |> write!(config.output)

    Path.relative_to_cwd(config.output)
  end

  defp normalize_config(config) do
    config
    |> Map.put(:output, Path.expand(config.output))
    |> Map.put(:name, (Project.config[:name] || config.project))
    |> Map.put(:description, Project.config[:description])
  end

  defp output_setup(config) do
    file_name = config.name |> String.downcase() |> Kernel.<>(".json")
    output = Path.join(config.output, file_name)

    if File.exists?(output) do
      File.rm!(output)
    else
      File.mkdir_p!(config.output)
    end

    %{config | output: output}
  end

  defp create_project_node(config, project_nodes) do
    %ExDoc.ProjectNode{
      name: config.name,
      version: config.version,
      homepage_url: config.homepage_url,
      description: config.description,
      icon: config.logo,
      items: %{
        modules: filter_by_type(:module, project_nodes),
        exceptions: filter_by_type(:exception, project_nodes),
        protocols: filter_by_type(:protocol, project_nodes),
        tasks: filter_by_type(:task, project_nodes),
        extras: HTML.build_extras(project_nodes, config, ".html"),
        attachments: extract_attachments_info(config),
      },
      language: config.language,
    }
  end

  defp filter_by_type(type, project_nodes) do
    type
    |> HTML.filter_list(project_nodes)
    |> Enum.map(fn mod ->
        mod =
          mod
          |> Map.from_struct()
          |> Map.merge(HTML.Templates.group_summary(mod))

        struct(ExDoc.LeanModuleNode, mod)
      end)
  end

  defp write!(content, output) do
    File.write!(output, content)
  end

  defp extract_attachments_info(config) do
    if path = config.assets do
      path
      |> Path.join("**/*")
      |> Path.wildcard()
      |> Enum.map(fn source ->
          %{
            path: Path.join("assets", Path.relative_to(source, path)),
            title: HTML.title_to_id(source),
            size_in_bytes: File.stat!(source).size,
          }
        end)
    else
      []
    end
  end
end
