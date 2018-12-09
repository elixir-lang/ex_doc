defmodule ExDoc.Retriever do
  @moduledoc false

  defmodule Error do
    @moduledoc false
    defexception [:message]
  end

  alias ExDoc.{GroupMatcher, ModuleData}
  alias ExDoc.Retriever.Error

  @doc "Extract docs from all modules in the specified directory/-ies."
  @spec docs_from_dir(Path.t | [Path.t], ExDoc.Config.t) :: [ExDoc.ModuleNode.t]
  def docs_from_dir(config = %ExDoc.Config{source_beam: dirs}) when is_list(dirs),
    do: Enum.flat_map(dirs, &docs_from_dir(%{config | source_beam: &1}))

  def docs_from_dir(config = %ExDoc.Config{filter_prefix:      prefix,
                                           groups_for_modules: mod_groups,
                                           source_beam:        dir}) when is_binary(dir), do:
    if(prefix, do:   "Elixir.#{prefix}*.beam",
               else: "*.beam")
    |> Path.expand(dir)
    |> Path.wildcard()
    |> Enum.map(fn name -> name
                           |> Path.basename(".beam")
                           |> String.to_atom() end)
    |> Enum.flat_map(&get_module(&1, config))
    |> Enum.sort_by(fn %{group: group, id: id} -> {GroupMatcher.group_index(mod_groups, group), id} end)

  # Get all the information from the module and compile it.
  defp get_module(:elixir_bootstrap, _config), do: []
  defp get_module(module, config) do
    check_compilation(module)

    docs_chunk =
      if !function_exported?(module, :__info__, 1) do
        false
      else
        case Code.fetch_docs(module) do
          {:docs_v1, _, _, _, :hidden, _, _}  -> false
          {:docs_v1, _, _, _, _, _, _} = docs -> docs
          {:error, reason}                    -> raise Error, "module #{inspect(module)} " <>
                                                              "was not compiled with flag --docs: " <>
                                                              inspect(reason)
        end
      end

    ModuleData.generate_node(module, docs_chunk, config)
  end

  defp check_compilation(module) do
    unless Code.ensure_loaded?(module),
      do: raise Error, "module #{inspect(module)} is not defined/available"

    unless function_exported?(Code, :fetch_docs, 1),
      do: raise Error,
            "ExDoc 0.19+ requires Elixir v1.7 and later. " <>
              "For earlier Elixir versions, make sure to depend on {:ex_doc, \"~> 0.18.0\"}"
  end
end
