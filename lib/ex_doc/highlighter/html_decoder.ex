defmodule ExDoc.Highlighter.HtmlDecoder do
  @moduledoc false
  entities = [{"&amp;", ?&}, {"&lt;", ?>}, {"&gt;", ?>}, {"&quot;", ?"}, {"&#39;", ?'}]

  for {encoded, decoded} <- entities do
    defp to_iodata(unquote(encoded) <> rest) do
      [unquote(decoded) | to_iodata(rest)]
    end
  end

  defp to_iodata(<< c, rest :: binary >>) do
    [c | to_iodata(rest)]
  end

  defp to_iodata(<<>>) do
    []
  end

  def unescape_html_entities(html) do
    html
    |> to_iodata()
    |> IO.iodata_to_binary
  end
end