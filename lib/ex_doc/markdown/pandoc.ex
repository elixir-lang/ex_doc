defmodule ExDoc.Markdown.Pandoc do
  @moduledoc """
  ExDoc extension for the Pandoc MarkDown parser
  """

  @doc """
  Check if the Pandoc MarkDown parser executable is available on the system
  """
  def available? do
    !!System.find_executable("pandoc")
  end

  @doc """
  Pandoc specific options:

    * `:format` - output format, defaults to :html
    * `:header_level` - base header level, outputs to 1

  """
  def to_html(text, opts \\ []) when is_binary(text) do
    text
    |> text_to_file()
    |> open_port(opts)
    |> process_port()
    |> pretty_codeblocks()
  end

  defp text_to_file(text) do
    id =
      4
      |> :crypto.rand_bytes()
      |> Base.encode16()
    unique_name = "tmpdoc_#{id}.md"
    tmp_path = Path.join(System.tmp_dir, unique_name)
    File.write!(tmp_path, text)
    tmp_path
  end

  defp open_port(path, opts) do
    exe  = :os.find_executable('pandoc')
    args = ["--from", "markdown+fenced_code_blocks",
            "--to", get_string(opts, :format, :html),
            "--base-header-level", get_string(opts, :header_level, 1),
            path]
    Port.open({:spawn_executable, exe}, [:stream, :binary, {:args, args},
              :hide, :use_stdio, :stderr_to_stdout, :exit_status])
  end

  defp get_string(opts, key, default) do
    opts
    |> Keyword.get(key, default)
    |> to_string()
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

  @doc false
  # Helper to handle fenced code blocks (```...```) with
  # language specification
  defp pretty_codeblocks(bin) do
    # Pandoc parser puts the class attribute inside the `pre` tag
    # Move the class attribute to the code element to keep consistency.
    bin = Regex.replace(~r/<pre\s+class=\"([^\"]+)\"><code>/,
                        bin, ~S(<pre><code class="\1">))

    bin
  end
end
