defmodule Markdown do
  @on_load { :init, 0 }

  def init do
    file = Path.expand('../../share/markdown', __FILE__)
    :ok = :erlang.load_nif(String.to_char_list!(file), 1)
  end

  def to_html(_) do
    exit(:nif_library_not_loaded)
  end
end
