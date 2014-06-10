defmodule ExDoc.Markdown.Sundown do
  @on_load { :init, 0 }

  def init do
    path = :filename.join(:code.priv_dir(:ex_doc), 'markdown')
    :erlang.load_nif(path, 1)
    :ok
  end

  def to_html(_text), do: nil

  def to_html(text, _header_lvl), do: to_html(text)
end
