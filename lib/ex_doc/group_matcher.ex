defmodule ExDoc.GroupMatcher do
  # General helpers for dealing with grouping functionality.
  # Extracted for organization and testability.
  @moduledoc false

  @type pattern :: Regex.t() | module() | String.t()
  @type patterns :: pattern | [pattern]
  @type group_patterns :: keyword(patterns)

  @doc """
  Finds the index of a given group.
  """
  def index(groups, group) do
    Enum.find_index(groups, fn {k, _v} -> k == group end) || -1
  end

  @doc """
  Finds a matching group for the given module name, id, and metadata.
  """
  def match_module(group_patterns, module, id, metadata) do
    match_group_patterns(group_patterns, fn pattern ->
      case pattern do
        %Regex{} = regex -> Regex.match?(regex, id)
        string when is_binary(string) -> id == string
        atom when is_atom(atom) -> atom == module
        function when is_function(function) -> function.(metadata)
      end
    end)
  end

  @doc """
  Finds a matching group for the given filename or url.
  """
  def match_extra(group_patterns, path) do
    match_group_patterns(group_patterns, fn pattern ->
      case pattern do
        %Regex{} = regex -> Regex.match?(regex, path)
        string when is_binary(string) -> path == string
      end
    end)
  end

  defp match_group_patterns(group_patterns, matcher) do
    Enum.find_value(group_patterns, fn {group, patterns} ->
      patterns = List.wrap(patterns)
      Enum.any?(patterns, matcher) && group
    end)
  end
end
