defmodule ExDoc.Markdown do
  @markdown_processors [ExDoc.Markdown.Hoedown, ExDoc.Markdown.Pandoc]
  @markdown_processor_key :markdown_processor

  def to_html(text) when is_binary(text) do
    get_markdown_processor().to_html(text)
  end

  defp get_markdown_processor() do
    case Application.fetch_env(:ex_doc, @markdown_processor_key) do
      {:ok, processor} -> processor
      :error ->
        processor = find_markdown_processor || raise_no_markdown_processor
        Application.put_env(:ex_doc, @markdown_processor_key, processor)
        processor
    end
  end

  defp find_markdown_processor() do
    Enum.find @markdown_processors, fn module ->
      Code.ensure_loaded?(module) && module.available?
    end
  end

  defp raise_no_markdown_processor() do
    raise """
    Could not find a markdown processor to be used on ex_doc.
    You can either:

    1. Add {:markdown, github: "devinus/markdown"} to your mix.exs deps
    2. Ensure pandoc (http://johnmacfarlane.net/pandoc) is available in your system
    """
  end
end
