defmodule ExDoc.GroupMatcher do
  @moduledoc """
  Match modules or extra pages to groups.

  Matching does happen by explicitly matching names or using regular expressions.
  """
  @type pattern :: Regex.t | module() | String.t
  @type patterns :: pattern | [pattern]
  @type group_patterns :: keyword(patterns)

  @doc """
  Does try to find a matching group for the given module name or id
  """
  @spec match_module(group_patterns, module(), String.t) :: String.t | nil
  def match_module(group_patterns, module, id) do
    match_group_patterns(group_patterns, fn pattern ->
      case pattern do
        %Regex{} = regex -> Regex.match?(regex, id)
        string when is_binary(string) -> id == string
        atom -> atom == module
      end
    end)
  end

  @doc """
  Does try to find a matching group for the given extra filename
  """
  @spec match_extra(group_patterns, String.t) :: String.t | nil
  def match_extra(group_patterns, filename) do
    match_group_patterns(group_patterns, fn pattern ->
      case pattern do
        %Regex{} = regex -> Regex.match?(regex, filename)
        string when is_binary(string) -> filename == string
      end
    end)
  end

  defp match_group_patterns(group_patterns, matcher) do
    Enum.find_value(group_patterns, fn {group, patterns} ->
      patterns = List.wrap patterns
      match_patterns(patterns, matcher) && Atom.to_string(group)
    end)
  end

  defp match_patterns(patterns, matcher) do
    Enum.any?(patterns, matcher) || nil
  end
end
