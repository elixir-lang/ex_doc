defmodule Markdown do
  @on_load { :init, 0 }

  def init do
    file = Path.expand('../../share/markdown', __FILE__)
    :ok = :erlang.load_nif(String.to_char_list!(file), 1)
  end

  def to_html(_) do
    exit(:nif_library_not_loaded)
  end

  def to_ansi(_) do
    exit(:nif_library_not_loaded)
  end

  @docs """
  Create links to locally defined functions as specified in `available_funs`.
  Ignores functions which are not defined in `available_funs`.
  `available_funs` is a list of strings of the form `fun/arity`.
  Ignores (mostly) functions which are already wrapped in markdown
  url syntax, eg `[test/1](url)`.
  Doesn't handle the case where an existing markdown url item contains
  a function but doesn't touch the leading or trailing ']',
  eg [my link link/1 is here](url); the link/1 will get translated to
  a new href to the function
  """
  def autolink_locals(bin, available_funs) when is_binary(bin) do
    matches = Regex.scan %r{(?<!\[)`\s*([a-z_!\\?]+/\d+)\s*`(?!\])}, bin
    [result] = 
      matches
      |> Enum.uniq
      |> List.flatten
      |> Enum.filter(&(&1 in available_funs))
      |> Enum.reduce([bin], fn (x, [acc]) ->
           escaped = Regex.escape(x)
           [Regex.replace(%r/(?<!\[)`(\s*(#{escaped})\s*)`(?!\])/, 
                          acc, "[`\\1`](#\\2)")]
         end)
    result
  end

end
