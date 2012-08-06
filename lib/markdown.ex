defmodule Markdown do

  def to_html(nil) do
    raise ArgumentError
  end
  def to_html(<<s, "\n">>) do
    convert(s)
  end
  def to_html(s) do
    convert(s)<>"\n"
  end

  defp convert(s) do
    s = :xmerl_ucs.from_utf8 s
    regex = case :erlang.get :url_regex do
      :undefined ->
        r = pattern
        :erlang.put(:url_regex, r)
        r
      v -> v
    end
    s = :markdown.conv(s)
    s = quote_urls(s, regex)
    list_to_binary :xmerl_ucs.to_utf8(s)
  end

  defp quote_urls(s, p) do
    case :re.run(s, p) do
      :nomatch -> s
      {:match, [{start, len}|_]} ->
        prev = :lists.sublist(s, 1, start)
        url = :lists.sublist(s, 1+start, len)
        url = '<a href="'++url++'">'++url++'</a>'
        nxt = if start+len < length(s) do
          quote_urls(:lists.sublist(s, 1+start+len, length(s)), p)
        else
          ''
        end
        prev++url++nxt
    end
  end

  defp pattern do
    {:ok, p} = :re.compile('(?i)\\b((?:[a-z][\\w-]+:(?:/{1,3}|[a-z0-9%])|www\\d{0,3}[.]|[a-z0-9.\\-]+[.][a-z]{2,4}/)(?:[^\\s()<>]+|\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\))+(?:\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\)|[^\\s`!()\\[\\]{};:\'\".,<>?«»\“\”\‘\’]))', [:unicode])
    p
  end
end
