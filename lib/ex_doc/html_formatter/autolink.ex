defmodule ExDoc.HTMLFormatter.Autolink do
  @moduledoc """
  Conveniences for autolinking locals, types and more.
  """

  @doc """
  Escape `'`, `"`, `&`, `<` and `>` in the string using HTML entities.
  This is only intended for use by the HTML formatter.
  """
  def escape_html(binary) do
    escape_map = [{ %r(&), "\\&amp;" }, { %r(<), "\\&lt;" }, { %r(>), "\\&gt;" }, { %r("), "\\&quot;" }]
    Enum.reduce escape_map, binary, fn({ re, escape }, acc) -> Regex.replace(re, acc, escape) end
  end

  @doc """
  Converts the given `ast` to string while linking the locals
  given by `typespecs`.
  """
  def typespec(ast, typespecs) do
    Macro.to_string(ast, fn
      { name, _, args }, string when is_atom(name) and is_list(args) ->
        arity = length(args)
        if { name, arity } in typespecs do
          %b[<a href="#t:#{name}/#{arity}">#{string}</a>]
        else
          string
        end
      _, string ->
        string
    end)
  end

  @doc """
  Create links to locally defined functions, specified in `available_funs`
  as a list of `fun/arity` tuples.

  Ignores functions which are already wrapped in markdown url syntax,
  e.g. `[test/1](url)`. In case the function doesn't touch the leading
  or trailing `]`, e.g. `[my link link/1 is here](url)`, the fun/arity
  will get translated to the new href of the function.
  """
  def docs(bin, available_funs) when is_binary(bin) do
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