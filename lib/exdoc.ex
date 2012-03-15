defmodule ExDoc do
  require Erlang.file, as: F

  def generate_docs(files, formatter // ExDoc::HTMLFormatter) do
    docs = ExDoc::Retriever.get_docs(files)
    move_css_files
    Enum.map docs, formatter.format_docs(&1)
  end

  ####
  # Helpers
  ####

  defp move_css_files() do
    css_path = File.expand_path("../../output/css", __FILE__)
    template_path = File.expand_path("../templates/css/main.css", __FILE__)

    F.make_dir(css_path)

    F.copy(template_path, css_path <> "/main.css")
  end
end
