defmodule ExDoc do
  require Erlang.file, as: F

  def generate_html(files) do
    docs = ExDoc::Retriever.get_docs(files)
    move_css_files
    Enum.map docs, ExDoc::HTMLWriter.write_html_to_file(&1)
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
