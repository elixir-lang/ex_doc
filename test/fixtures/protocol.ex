defprotocol CustomProtocol do
  def plus_one(foo)
  def plus_two(bar)
end

defimpl CustomProtocol, for: Number do
  @doc """
  Special plus one docs
  """
  def plus_one(number), do: number + 1
  def plus_two(number), do: number + 2
end
