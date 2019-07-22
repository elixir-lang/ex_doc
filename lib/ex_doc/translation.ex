defmodule ExDoc.Translation do
  @moduledoc false

  def translate(config, text),
    do: t(config.language, text)

  def t(locale, text) when is_atom(text),
    do: t(locale, Atom.to_string(text))

  # Locales

  def t("es", string),
    do: ExDoc.Translation.Es.t(string)

  # More locales ...

  # Catch-all clause
  def t(_, text),
    do: text
end
