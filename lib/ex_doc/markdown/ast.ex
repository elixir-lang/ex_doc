defmodule ExDoc.Markdown.AST do
  @type html :: [html_element()]

  @type html_element :: {html_tag(), metadata(), html_attributes(), children()} | String.t()
  @type html_tag :: atom()
  @type metadata :: %{
          optional(atom()) => any(),
          optional(:line) => integer(),
          optional(:column) => integer(),
          optional(:verbatim) => boolean(),
          optional(:comment) => boolean()
        }
  @type html_attributes :: Keyword.t(String.t())
  @type children :: html()

  # https://www.w3.org/TR/2011/WD-html-markup-20110113/syntax.html#void-element
  @void_elements ~W(area base br col command embed hr img input keygen link meta param source track wbr)a

  # Ported from: Earmark.Transform
  # Copyright (c) 2014 Dave Thomas, The Pragmatic Programmers @/+pragdave, dave@pragprog.com
  # Apache License v2.0
  # https://github.com/pragdave/earmark/blob/a2a85bc3f2e262a2c697ed8001b0eaa06ee42d92/lib/earmark/transform.ex
  @spec to_html(html()) :: String.t()
  def to_html(ast, options \\ %{})

  def to_html(ast, options) do
    ast
    |> to_html(options, false)
    |> IO.iodata_to_binary()
  end

  defp to_html(elements, options, verbatim) when is_list(elements) do
    Enum.map(elements, &to_html(&1, options, verbatim))
  end

  defp to_html(element, options, false) when is_binary(element) do
    case escape_with_options(element, options) do
      "" ->
        []

      content ->
        [content]
    end
  end

  defp to_html(element, _options, true) when is_binary(element) do
    [element]
  end

  # Void element
  defp to_html({tag, _metadata, attributes, []}, _options, _verbatim) when tag in @void_elements,
    do: open_element(tag, attributes)

  # Comment
  defp to_html({nil, %{comment: true}, _attributes, children}, _options, _verbatim) do
    ["<!--", Enum.intersperse(children, ["\n"]), "-->"]
  end

  defp to_html({:code, _metadata, attributes, children}, _options, __verbatim) do
    [
      open_element(:code, attributes),
      children |> Enum.join("\n") |> escape(true),
      "</code>"
    ]
  end

  defp to_html({:pre, metadata, attributes, children}, options, verbatim) do
    verbatim_new = metadata[:verbatim] || verbatim

    [
      open_element(:pre, attributes),
      to_html(Enum.intersperse(children, ["\n"]), options, verbatim_new),
      "</pre>\n"
    ]
  end

  # Element with no children
  defp to_html({tag, _metadata, attributes, []}, _options, _verbatim) do
    [open_element(tag, attributes), "</#{tag}>", "\n"]
  end

  # Element with children
  defp to_html({tag, metadata, attributes, children}, options, verbatim) do
    verbatim_new = metadata[:verbatim] || verbatim

    [open_element(tag, attributes), to_html(children, options, verbatim_new), "</#{tag}>"]
  end

  defp make_attribute(name_value_pair, tag)

  defp make_attribute({name, value}, _) do
    [" ", "#{name}", "=\"", to_string(value), "\""]
  end

  defp open_element(tag, attributes) when tag in @void_elements do
    ["<", "#{tag}", Enum.map(attributes, &make_attribute(&1, tag)), " />"]
  end

  defp open_element(tag, attributes) do
    ["<", "#{tag}", Enum.map(attributes, &make_attribute(&1, tag)), ">"]
  end

  @em_dash_regex ~r{---}
  @en_dash_regex ~r{--}
  @dbl1_regex ~r{(^|[-–—/\(\[\{"”“\s])'}
  @single_regex ~r{\'}
  @dbl2_regex ~r{(^|[-–—/\(\[\{‘\s])\"}
  @dbl3_regex ~r{"}
  defp smartypants(text, options)

  defp smartypants(text, %{smartypants: true}) do
    text
    |> replace(@em_dash_regex, "—")
    |> replace(@en_dash_regex, "–")
    |> replace(@dbl1_regex, "\\1‘")
    |> replace(@single_regex, "’")
    |> replace(@dbl2_regex, "\\1“")
    |> replace(@dbl3_regex, "”")
    |> String.replace("...", "…")
  end

  defp smartypants(text, _options), do: text

  defp replace(text, regex, replacement, options \\ []) do
    Regex.replace(regex, text, replacement, options)
  end

  # Originally taken from: Earmark.Helpers
  # Copyright (c) 2014 Dave Thomas, The Pragmatic Programmers @/+pragdave, dave@pragprog.com
  # Apache License v2.0
  # https://github.com/pragdave/earmark/blob/a2a85bc3f2e262a2c697ed8001b0eaa06ee42d92/lib/earmark/helpers.ex
  # 
  # Replace <, >, and quotes with the corresponding entities. If
  # `encode` is true, convert ampersands, too, otherwise only
  #  convert non-entity ampersands.
  def escape(html, encode \\ false)

  def escape(html, false) when is_binary(html),
    do: escape_replace(Regex.replace(~r{&(?!#?\w+;)}, html, "&amp;"))

  def escape(html, _) when is_binary(html), do: escape_replace(String.replace(html, "&", "&amp;"))

  defp escape_replace(html) do
    html
    |> String.replace("<", "&lt;")
    |> String.replace(">", "&gt;")
    |> String.replace("\"", "&quot;")
    |> String.replace("'", "&#39;")
  end

  defp escape_with_options(element, options)

  defp escape_with_options("", _options),
    do: ""

  defp escape_with_options(element, options) do
    element
    |> smartypants(options)
    |> escape()
  end
end
