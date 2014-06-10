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

  def to_html(text, header_lvl) when is_binary(text) do
    tmp_path = text_to_file(text)
    convert_markdown(tmp_path, "html", header_lvl)
  end

  defp text_to_file(text) do
    {a,b,c} = :erlang.now()
    d = :erlang.phash2(text)  # FIXME
    unique_name = "tmpdoc_#{a}#{b}#{c}_#{d}.md"
    tmp_path = Path.join(System.tmp_dir, unique_name)

    File.write!(tmp_path, text)
    tmp_path
  end

  @doc false
  # Used by custom formatters that need to get intermediate markup (e.g. reST)
  # before generating final HTML
  def convert_markdown(path, format, header_lvl) do
    open_port(path, format, header_lvl) |> process_port()
  end

  defp open_port(path, format, header_lvl) do
    exe = :os.find_executable('pandoc')
    args = ["--from", "markdown",
            "--to", format,
            "--base-header-level", to_string(header_lvl),
            path]
    Port.open({:spawn_executable, exe}, [:stream, :binary, {:args, args},
                                   :use_stdio, :stderr_to_stdout, :exit_status])
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
end
