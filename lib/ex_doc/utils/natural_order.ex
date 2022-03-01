defmodule ExDoc.Utils.NaturalOrder do
  @moduledoc false

  def to_sortable_charlist(string) do
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

  # Then underscore
  defp make_sortable([?_ | chars]), do: [?0 | make_sortable(chars)]

  # Then uppercased letters and lowercased letters
  defp make_sortable([char | chars]) when char in ?a..?z do
    [char - 31.5 | make_sortable(chars)]
  end

  defp make_sortable([char | chars]), do: [char | make_sortable(chars)]
  defp make_sortable([]), do: []
end
