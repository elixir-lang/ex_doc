defmodule ExDoc.Retriever do
  def get_docs(files, relative_to) do
    Enum.map files, get_docs_from_file(&1, "#{relative_to}/__MAIN__")
  end

  # Helpers

  defp get_docs_from_file(file, relative_to) do
    module_name = get_module_name file, relative_to
    module = :"__MAIN__.#{module_name}"

    moduledoc = module.__info__(:moduledoc)
    docs = Enum.filter module.__info__(:docs), has_doc?(&1)

    { module_name, { moduledoc, docs } }
  end

  defp has_doc?({_, _, _, false}) do
    false
  end

  defp has_doc?({_, _, _, _doc}) do
    true
  end

  defp get_module_name(name, relative_to) when is_list(name) do
    get_module_name list_to_binary(name), relative_to
  end

  defp get_module_name(name, relative_to) do
    name = File.split :filename.rootname(name, '.beam')
    relative = File.split relative_to
    hierarchy = :lists.subtract name, relative
    Enum.join hierarchy, "."
  end
end
