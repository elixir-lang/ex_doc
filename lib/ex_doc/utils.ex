defmodule ExDoc.Utils do
  @moduledoc false

  @doc """
  Emits a warning.
  """
  if Version.match?(System.version(), ">= 1.14.0") do
    def warn(message, stacktrace_info) do
      IO.warn(message, stacktrace_info)
    end
  else
    def warn(message, _stacktrace_info) do
      IO.warn(message, [])
    end
  end

  @doc """
  Runs the `before_closing_head_tag` callback.
  """
  def before_closing_head_tag(%{before_closing_head_tag: {m, f, a}}, module) do
    apply(m, f, [module | a])
  end

  def before_closing_head_tag(%{before_closing_head_tag: before_closing_head_tag}, module)
      when is_map(before_closing_head_tag) do
    Map.get(before_closing_head_tag, module, "")
  end

  def before_closing_head_tag(%{before_closing_head_tag: before_closing_head_tag}, module) do
    before_closing_head_tag.(module)
  end

  @doc """
  Runs the `before_closing_footer_tag` callback.
  """
  def before_closing_footer_tag(%{before_closing_footer_tag: {m, f, a}}, module) do
    apply(m, f, [module | a])
  end

  def before_closing_footer_tag(%{before_closing_footer_tag: before_closing_footer_tag}, module)
      when is_map(before_closing_footer_tag) do
    Map.get(before_closing_footer_tag, module, "")
  end

  def before_closing_footer_tag(%{before_closing_footer_tag: before_closing_footer_tag}, module) do
    before_closing_footer_tag.(module)
  end

  @doc """
  Runs the `before_closing_body_tag` callback.
  """
  def before_closing_body_tag(%{before_closing_body_tag: {m, f, a}}, module) do
    apply(m, f, [module | a])
  end

  def before_closing_body_tag(%{before_closing_body_tag: before_closing_body_tag}, module)
      when is_map(before_closing_body_tag) do
    Map.get(before_closing_body_tag, module, "")
  end

  def before_closing_body_tag(%{before_closing_body_tag: before_closing_body_tag}, module) do
    before_closing_body_tag.(module)
  end

  @doc """
  HTML escapes the given string.

      iex> ExDoc.Utils.h("<foo>")
      "&lt;foo&gt;"

  """
  def h(string) do
    String.replace(string, ~w|& < > "|, fn
      "&" -> "&amp;"
      "<" -> "&lt;"
      ">" -> "&gt;"
      ~S(") -> "&quot;"
    end)
  end

  @doc """
  Sorts mapped strings by natural order.
  """
  def natural_sort_by(enumerable, mapper) when is_function(mapper, 1) do
    Enum.sort_by(enumerable, fn elem ->
      elem
      |> mapper.()
      |> to_sortable_charlist()
    end)
  end

  defp to_sortable_charlist(string) do
    string
    |> :unicode.characters_to_nfkd_list()
    |> make_sortable()
  end

  @offset -1_000_000_000

  # Numbers come first, so group and pad them with offset
  defp make_sortable([digit | chars]) when digit in ?0..?9 do
    {digits, chars} = Enum.split_while(chars, &(&1 in ?0..?9))
    [@offset + List.to_integer([digit | digits]) | make_sortable(chars)]
  end

  # Then Elixir special punctuation - trailing bang `!`
  defp make_sortable([?! | chars]), do: [?0 | make_sortable(chars)]

  # Then Elixir special punctuation - question mark `?`
  defp make_sortable([?? | chars]), do: [?1 | make_sortable(chars)]

  # Then underscore
  defp make_sortable([?_ | chars]), do: [?2 | make_sortable(chars)]

  # Then uppercased letters and lowercased letters
  defp make_sortable([char | chars]) when char in ?a..?z do
    [char - 31.5 | make_sortable(chars)]
  end

  defp make_sortable([char | chars]), do: [char | make_sortable(chars)]
  defp make_sortable([]), do: []

  @doc """
  A very simple JSON encoder.

  We want to minimize the number of dependencies ExDoc has,
  because we don't want someone to be allowed to not upgrade
  their app due to an ExDoc restriction, so we ship with a
  simple JSON implementation.
  """
  def to_json(nil), do: "null"
  def to_json(true), do: "true"
  def to_json(false), do: "false"

  def to_json(map) when is_map(map) do
    mapped =
      Enum.map_intersperse(Map.to_list(map), ?,, fn {key, value} ->
        [key |> Atom.to_string() |> inspect(), ?:, to_json(value)]
      end)

    [?{, mapped, ?}]
  end

  def to_json(list) when is_list(list) do
    mapped = Enum.map_intersperse(list, ?,, &to_json/1)
    [?[, mapped, ?]]
  end

  def to_json(atom) when is_atom(atom) do
    atom |> Atom.to_string() |> inspect()
  end

  def to_json(binary) when is_binary(binary) do
    to_json_string(binary, "\"")
  end

  def to_json(integer) when is_integer(integer) do
    Integer.to_string(integer)
  end

  defp to_json_string(<<?\b, rest::binary>>, acc),
    do: to_json_string(rest, <<acc::binary, "\\b">>)

  defp to_json_string(<<?\t, rest::binary>>, acc),
    do: to_json_string(rest, <<acc::binary, "\\t">>)

  defp to_json_string(<<?\n, rest::binary>>, acc),
    do: to_json_string(rest, <<acc::binary, "\\n">>)

  defp to_json_string(<<?\f, rest::binary>>, acc),
    do: to_json_string(rest, <<acc::binary, "\\f">>)

  defp to_json_string(<<?\r, rest::binary>>, acc),
    do: to_json_string(rest, <<acc::binary, "\\r">>)

  defp to_json_string(<<?\\, rest::binary>>, acc),
    do: to_json_string(rest, <<acc::binary, "\\\\">>)

  defp to_json_string(<<?", rest::binary>>, acc),
    do: to_json_string(rest, <<acc::binary, "\\\"">>)

  defp to_json_string(<<x, rest::binary>>, acc) when x <= 0x000F,
    do: to_json_string(rest, <<acc::binary, "\\u000", Integer.to_string(x, 16)::binary>>)

  defp to_json_string(<<x, rest::binary>>, acc) when x <= 0x001F,
    do: to_json_string(rest, <<acc::binary, "\\u00", Integer.to_string(x, 16)::binary>>)

  defp to_json_string(<<x, rest::binary>>, acc),
    do: to_json_string(rest, <<acc::binary, x>>)

  defp to_json_string(<<>>, acc), do: <<acc::binary, "\"">>

  @doc """
  Generates a url based on the given pattern.
  """
  def source_url_pattern(source_url_pattern, path, line)
      when is_binary(path) and is_integer(line) do
    cond do
      is_function(source_url_pattern) ->
        source_url_pattern.(path, line)

      source_url_pattern ->
        source_url_pattern
        |> String.replace("%{path}", path)
        |> String.replace("%{line}", Integer.to_string(line))

      true ->
        nil
    end
  end
end
