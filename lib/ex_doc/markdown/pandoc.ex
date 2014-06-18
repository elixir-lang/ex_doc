defmodule ExDoc.Markdown.Pandoc do
  def init do
    if :os.find_executable('pandoc') do
      ExDoc.register_markdown_processor(__MODULE__)
    else
      {:error, "pandoc program not found"}
    end
  end


  def to_html(text, header_lvl \\ 1)

  def to_html(nil, _), do: raise(ArgumentError, message: "Expected a binary")

  def to_html(text, header_lvl) do
    convert_markdown(text, "html", header_lvl)
  end

  @doc false
  # Used by custom formatters that need to get intermediate markup (e.g. reST)
  # before generating final HTML
  def convert_markdown(text, format, header_lvl) when is_binary(text) do
    tmp_path = text_to_file(text)
    open_port(tmp_path, format, header_lvl) |> process_port()
  end

  defp text_to_file(text) do
    id = :crypto.rand_bytes(4) |> bin_to_hex
    unique_name = "tmpdoc_#{id}.md"
    tmp_path = Path.join(System.tmp_dir, unique_name)

    File.write!(tmp_path, text)
    tmp_path
  end

  defp open_port(path, format, header_lvl) do
    exe = :os.find_executable('pandoc')
    args = ["--from", "markdown",
            "--to", format,
            "--base-header-level", to_string(header_lvl),
            path]
    Port.open({:spawn_executable, exe}, [:stream, :binary, {:args, args},
                            :hide, :use_stdio, :stderr_to_stdout, :exit_status])
  end

  defp process_port(port) do
    {0, data} = collect_output(port, [])
    data
  end

  defp collect_output(port, data) do
    receive do
      {^port, {:data, new_data}} ->
        collect_output(port, [data, new_data])

      {^port, {:exit_status, status}} ->
        {status, List.to_string(data)}
    end
  end

  defp bin_to_hex(bin), do: bin_to_hex(bin, <<>>)

  defp bin_to_hex(<<>>, acc), do: acc
  defp bin_to_hex(<<hi::4, lo::4>> <> rest, acc),
    do: bin_to_hex(rest, <<hex_char(hi), hex_char(lo), acc::binary>>)

  defp hex_char(n) when n in 0..9, do: ?0 + n
  defp hex_char(n), do: ?a + n - 10
end
