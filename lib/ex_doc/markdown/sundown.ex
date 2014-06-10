defmodule ExDoc.Markdown.Sundown do
  # We have to call init_nif on module load to ensure there is only ever one
  # call made to the :erlang.load_nif() function after NIF loading has
  # succeeded.
  #
  # If loading fails, load_nif will be called again from `init/0` to get to the
  # error message.
  @on_load {:init_nif, 0}

  def init do
    if available? do
      ExDoc.register_markdown_processor(__MODULE__)
    else
      init_nif(true)
    end
  end

  defp init_nif(should_return_error \\ false) do
    path = :filename.join(:code.priv_dir(:ex_doc), 'markdown')
    case :erlang.load_nif(path, 1) do
      {:error, _}=error ->
        if should_return_error, do: error, else: :ok
      :ok -> :ok
    end
  end

  def to_html(_text), do: nil

  def to_html(text, _header_lvl), do: to_html(text)

  @doc false
  def available?, do: false
end
