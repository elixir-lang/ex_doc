defmodule ExDoc.Translation do
  @moduledoc false

  def translate(config, text, args \\ []),
    do: t(config.language, text, args)

  defp t(locale, text, args) when is_atom(text),
    do: t(locale, Atom.to_string(text), args)

  # Locales

  defp t("es", string, []),
    do: ExDoc.Translation.Es.t(string)

  defp t("es", string, args),
    do: ExDoc.Translation.Es.t(string, args)

  # More locales ...

  # Catch-all clause
  defp t(_locale, text, []),
    do: text

  defp t(_locale, text, args) when is_list(args) do
    replace(text, args)
  end

  @doc ~S"""
  Replaces `text` with the values passed in the `args` keyword list.

  Named arguments are represented in the text in the shape of `\#{key}`.

  ## Examples

      iex> ExDoc.Translation.replace("\#{name} is my name and \#{last_name} is my last name.", [name: "John", last_name: "Doe"])
      "John is my name and Doe is my last name."

  """
  @spec replace(String.t(), keyword(String.t())) :: String.t()
  def replace(text, []) do
    text
  end

  # TODO: once minimum supported version is Elixir v1.9 or higher, replace this clause with
  # def replace(text, args) when is_list(args) do
  #   search = Enum.map(args, fn {k, _v} -> "\#{#{k}}" end)
  #   replace = Enum.map(args, fn {k, v} -> {:"\#{#{k}}", v} end)
  # 
  #   String.replace(
  #     text,
  #     :binary.compile_pattern(search),
  #     &Keyword.get(replace, String.to_atom(&1))
  #   )
  # end
  def replace(text, args) when is_list(args) do
    Enum.reduce(args, text, fn {k, v}, string -> String.replace(string, "\#{#{k}}", v) end)
  end
end
