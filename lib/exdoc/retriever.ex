defmodule ExDoc.Retriever do
  def get_docs(files) do
    Enum.map files, get_docs_from_file(&1)
  end

  # Helpers

  defp get_docs_from_file(file) do
    module_name = get_module_name(file)
    module = :"__MAIN__.#{module_name}"

    moduledoc = module.__info__(:moduledoc)
    docs = module.__info__(:docs)

    { module_name, { moduledoc, docs } }
  end

  defp get_module_name(name) do
    File.basename(name, '.beam')
  end
end
