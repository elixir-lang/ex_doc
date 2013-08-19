defmodule ExDoc.Autolink do
  @moduledoc """
  Conveniences for autolinking locals, types and more.
  """

  @doc """
  Create links to locally defined functions, specified in `available_funs`
  as a list of `fun/arity` tuples.

  Ignores functions which are already wrapped in markdown url syntax,
  e.g. `[test/1](url)`. In case the function doesn't touch the leading
  or trailing `]`, e.g. `[my link link/1 is here](url)`, the fun/arity
  will get translated to the new href of the function.
  """
  def locals(bin, available_funs) when is_binary(bin) do
    Regex.scan(%r{(?<!\[)`\s*([a-z_!\\?]+/\d+)\s*`(?!\])}, bin)
    |> Enum.uniq
    |> List.flatten
    |> Enum.filter(&(&1 in available_funs))
    |> Enum.reduce(bin, fn (x, acc) ->
         escaped = Regex.escape(x)
         Regex.replace(%r/(?<!\[)`(\s*(#{escaped})\s*)`(?!\])/, acc, "[`\\1`](#\\2)")
       end)
  end
end