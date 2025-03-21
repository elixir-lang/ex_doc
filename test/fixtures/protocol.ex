defprotocol CustomProtocol do
  @moduledoc """
  See `plus_one/1`.
  """

  def plus_one(foo)
  def plus_two(bar)
end

defimpl CustomProtocol, for: Integer do
  @doc """
  Special plus one docs
  """
  def plus_one(int), do: int + 1
  def plus_two(int), do: int + 2
end
